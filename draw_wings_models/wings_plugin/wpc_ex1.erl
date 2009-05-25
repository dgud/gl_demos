%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : wpc_ex1.erl
%%% Author  : Dan Gudmundsson  dangud(a)gmail.com
%%%
%%% Description : An example of how to write simple exporter plugin to wings
%%%               It's a binary export of all triangles it includes normals and
%%%               uv-coordinates.
%%%
%%%               For an example of how to create textual format take look at
%%%               wpc_wrl.erl which is pretty short.
%%%
%%% Created : 11 Mar 2005 by Dan Gudmundsson
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Welcome to the wonderful world of erlang boys and girls.
%% Put this file in dir wings/plugins_src/import_export/
%% Add the file to the Makefile 
%% invoke make and restart wings..and voila a new exporter have appeared

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fileformat of the created files:
%%
%%  Everything is 32 bits little endian, integers and floats
%%  all 'SizeOf' contains the number of bytes
%%  The 'Size Of Name' includes an ending \0
%%
%%  We start of with a header chunk of 128 bytes size, it contains:
%%  a MAGICNUMBER, 2 spare bytes, Size of RawVertexChunk  and extra info bytes
%%  16#E15E 16#0000 TotSizeOfVertexData + 120 bytes of creation info
%%
%%  Directly after the header comes 'TotSizeOfVertexData' bytes.
%%  It contains raw vertex data chunk for every object. 
%%     (each triangle looks like)
%%     V1x,V1y,V1z,N1x,N1y,N1z,S1,T1
%%     V2x,V2y,V2z,N2x,N2y,N2z,S2,T2
%%     V3x,V3y,V3z,N3x,N3y,N3z,S3,T3
%%  i.e. each Vertex have 8*4=32 bytes of info and each triangle have 32*3 bytes.
%% 
%%  After the vertexchunk comes a materialchunk it looks like this: 
%%  SizeOfMaterialChunk (in bytes, 32bits little endian)
%%  SizeOfMatname MatName\0
%%          DiffuseRGBA  
%%          AmbientRGBA  
%%          SpecularRGBA 
%%          EmissionRGBA 
%%          Shininess (float between 0.0-1.0)
%%          NumberOfTextureImages     
%%             SizeOfImageTypeName ImageTypeName\0 SizeOfFileName FileName\0
%%             SizeOfImageTypeName ImageTypeName\0 SizeOfFileName FileName\0
%%           Where ImageType is string describing the file i.e. diffuse,gloss,bump
%%  SizeOfMatName MatName2\0
%%     ....
%%
%%  And the last block contains object information, each object is separated
%%  per material, so each object may contain several parts.
%%  A StartVertex of Zero means the that the block starts at VertexChunk
%%  
%%  TotSizeOfObjectChunk
%%  SizeOfThisObjectChunk SizeOfObjectName ObjectName\0 
%%      NumberOfMeshesInObject
%%        SizeOfMatName MatName\0 StartVertex NoOfVertices
%%        SizeOfMatName MatName\0 StartVertex NoOfVertices
%%        ....
%%  SizeOfThisObjectChunk SizeOfObjectName ObjectName\0 
%%      NumberOfMeshesInObject
%%        SizeOfMatName MatName\0 StartVertex NoOfVertices
%%        SizeOfMatName MatName\0 StartVertex NoOfVertices
%%        ....
%%
%%  Comments about the format, I have choosen this because if the user
%%  ignores everything about materials and objects, it's very simple
%%  to parse. Read vertexchunk size in bytes 4-8 (an integer) skip
%%  120 bytes and read the vertex chunk and skip everything after 
%%  the vertexchunk size of bytes.

%%  I used the following (erlang) opengl vbo code when testing the exporter:
%%
%%     [Buff] = gl:genBuffers(1),
%%     gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
%%     gl:bufferData(?GL_ARRAY_BUFFER, size(DataChunk), DataChunk, ?GL_STATIC_DRAW),
%%     gl:vertexPointer(3, ?GL_FLOAT,  8*4, 0),
%%     gl:normalPointer(?GL_FLOAT,     8*4, 3*4),
%%     gl:texCoordPointer(2,?GL_FLOAT, 8*4, 6*4),
%%    
%%     %% The acutal Drawing code is
%%	gl:enableClientState(?GL_VERTEX_ARRAY),
%%	gl:enableClientState(?GL_NORMAL_ARRAY),
%%	gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
%%	foreach(fun({Mat,First,Sz}) ->
%%		 set_mat(Mat,Mats),
%%		 gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
%%		 gl:drawArrays(?GL_TRIANGLES, First, Sz),
%%		 gl:bindBuffer(?GL_ARRAY_BUFFER,0)
%%	        end, ObjRefs),
%%	gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
%%	gl:disableClientState(?GL_VERTEX_ARRAY),
%%	gl:disableClientState(?GL_NORMAL_ARRAY)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Now lets get on with the implementation of the exporter.

%% Module declaration, it defines the name of the this module.
%% and how it is accessed from other modules, must be the same
%% as the filename. Also for wings to find plugin it must start
%% with 'wpc_'
-module(wpc_ex1).

%% -export() defines which functions can be accessed from
%% other modules.
%% These are the functions that wings will call and must be exported
-export([init/0, menu/2, command/2]).

%% Importing can viewed as a shortcut that way we don't have write
%% the module 'lists' before each call to foreach,foldl,map
-import(lists, [foreach/2, foldl/3, map/2, reverse/1]).
%% proplists is really long to type import the used function
-import(proplists, [get_value/2,get_value/3]).

%% Some include files.
-include("e3d.hrl").
-include("e3d_image.hrl").

%% A define loooks like this.
-define(EXTENSION, wex1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Wings callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is first function that wings will call when wings starts, we
%% don't have to do any initialization work, just return true.
init() ->
   true.

%% Menu, these functions will be called each time the user opens
%% a menu from wings.

%% We want to add a menu entry in export menu at the bottom
menu({file, export}, Menu) ->
   Menu ++ [{"Example exporter (.wex1)...", ?EXTENSION, [option]}];
%% We don't care about other menues.
menu(_, Menu) -> Menu.

%% Command, these functions will be invoked by wings when a command
%% is invoke from the user, either by a menu or shortcut.

%% Lets export the objects if our command was invoked.
command({file,{export,{?EXTENSION,Ask}}}, St) ->
   init_export(Ask, St);
%% Skip all other commands..
command(_, _) -> next.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exporter initialization functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% If Ask is an atom the user wants to edit the options
%% open the option dialog.

%% We also define and give wpa:dialog a function to be invoked
%% with the result, that function just restarts the export with the
%% options.
init_export(Ask, _) when is_atom(Ask) ->
   Result = fun(Options) ->
                    {file,{export,{?EXTENSION,Options}}}
            end,
   %% We want tga to be the default texture image format
   wpa:pref_set_default(?MODULE, default_filetype, ".tga"),
   %% We want to use the standard export dialog + a compress option
   %% but we don't want include(uvs/normal/vertexcolor) options exclude them 
   ExcludeOptions = [include_uvs,include_normals, include_colors],
   Dialog = [wpa:dialog_template(?MODULE, export,ExcludeOptions)],
   %% Everything done display the dialog
   wpa:dialog(Ask, "Export Options", Dialog, Result);

%% Ok we where invoked with some export options, the options
%% where either set with the dialog or picked from the preferences.
init_export(Options, St) ->
   %% Store the options in the preferences, so that next time it will
   %% be the default options.
   wpa:pref_set(?MODULE, Options),

   %% get_value/3 is an imported function (from proplists)
   SubDivs = get_value(subdivisions, Options, 0),
   %% Create the export options
   ExportOpts = [{subdivisions, SubDivs},
                 {tesselation,triangulate},
                 {ext, ".wex1"},{ext_desc, "Example exporter"}],

   %% Create the callback function which will be called from wings,
   %% it takes 2 arguments FileName and Contents
   ExportFun =
       fun(Filename, Contents) ->
               export_file(Filename, Contents, Options)
       end,
   %% Call wpa:export which will subdivide and triangulate the meshes
   %% and convert wings internal datastructures to a e3d mesh (see
   %% e3d.hrl) then it will call the callback function 'ExportFun'
   wpa:export(ExportOpts, ExportFun, St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  The real export code comes here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_file(Filename, Data0 = #e3d_file{}, Options) ->
   %% Transform data i.e. scale and/or rotate of Z
   Transform = wpa:export_matrix(Options),
   Data1 = e3d_file:transform(Data0, Transform),

   %% Export all textures..
   Filetype = get_value(default_filetype, Options, ".tga"),
   Data = wpa:save_images(Data1, filename:dirname(Filename), Filetype),

   %% Get the stuff we need from Data
   #e3d_file{objs=Objs,mat=Materials,creator=Creator} = Data,

   try
       %% Create the raw mesh chunks
       {ObjInfo,ObjData} =  %% Foreach Object in objs 
               foldl(fun(#e3d_object{obj=Object,name=Name}, {ObjInfo,ObjData}) ->
			     %% Calculate smooth normals per vertex
			     ObjWithNormals = e3d_mesh:vertex_normals(Object),
			     %% Split the object per material
			     Meshes = e3d_mesh:split_by_material(ObjWithNormals),
			     %% Foreach Mesh in Meshes, create the Info and data
			     {MeshesInfo,MeshesData} 
				 = foldl(fun(Mesh, {MIIn,MDin}) ->
						 %% Each object have Info and Data
						 {MI,MD} = export_object(Mesh),
						 %% Add it to the rest
						 {[MI|MIIn],[MD|MDin]}
					 end, 
					 {[],ObjData}, %% Output params
					 Meshes), %% Loop over all meshes.
			     %% Rember the Name and MeshesInfo and the data
			     {[{Name,MeshesInfo}|ObjInfo],MeshesData}
		     end, {[],[]}, Objs),

       %% Add all meshes together
       DataChunk = list_to_binary(ObjData),
       
       %% Convert Creator string to binary
       CreatorBin  = list_to_binary(Creator),
       %% Create the header chunk, init bytes, create info and fill up with \0
       HeaderChunk = <<16#E15E:16/little,16#0000:16/little,(size(DataChunk)):32/little,
                      CreatorBin/binary, 0:((120-size(CreatorBin))*8)>>,
       
       %% Create the material chunk
       MaterialChunk = export_materials(Materials,[]),
       %% Create Object reference chunk
       ObjectRefChunk = export_object_refs(ObjInfo, 0, []),

       %% Add it all together..
       FileContents = <<HeaderChunk/binary,DataChunk/binary,
		       (size(MaterialChunk)):32/little,MaterialChunk/binary,
		       (size(ObjectRefChunk)):32/little,ObjectRefChunk/binary>>,
       %% And write the lot to the file
       ok = file:write_file(Filename, FileContents)

   catch  %% Some error handling
       throw:Error ->
           io:format("Exporter failed: ~p~n", [Error]),
           ErrStr = io_lib:format("Exporter failed: ~s", [Error]),
           wings_u:error(lists:flatten(ErrStr));
         _:Error ->
           io:format("Exporter crashed: ~p ~p~n", [Error, erlang:get_stacktrace()]),
           ErrStr = io_lib:format("Exporter crashed: ~W", [Error,4]),
           wings_u:error(lists:flatten(ErrStr))
   end.

%% At last we can see the end of everything..
export_object(#e3d_mesh{vs=Vs0,ns=Ns0,tx=Uv0,fs=Fs}) ->
    %% Do some initialization,
    %% convert the lists to a tuple of floats
    Vs = convert_to_binary(Vs0, []),
    Ns = convert_to_binary(Ns0, []),
    Uv = convert_to_binary(Uv0, []),
    %% Here we go again for each face in Fs
    %% Create a binary that looks like
    %% <<V1x,V1y,V1z,N1x,N1y,N1z,U1,V1
    %%   V2x,V2y,V2z,N2x,N2y,N2z,U2,V2
    %%   V3x,V3y,V3z,N3x,N3y,N3z,U3,V3>>
    ListOfBinFs =
	map(fun(#e3d_face{vs=[V1,V2,V3],ns=[N1,N2,N3],tx=[UV1,UV2,UV3]}) ->
		    <<(idx(V1,Vs))/binary,(idx(N1,Ns))/binary,(idx(UV1,Uv))/binary,
		     (idx(V2,Vs))/binary,(idx(N2,Ns))/binary,(idx(UV2,Uv))/binary,
		     (idx(V3,Vs))/binary,(idx(N3,Ns))/binary,(idx(UV3,Uv))/binary>>;
	       (#e3d_face{tx=[]}) ->
		    throw("No UV coordinates available");
	       (_) -> % Proberly not triangulated correctly
		    throw("Model not triangulated, report BUG in wings trianglutor")
	    end, Fs),
    %% So we got a list of binaries convert that to a big binary..
    BinFs = list_to_binary(ListOfBinFs),
    
    %% Get the Material name since we have done a split by material
    %% each face will have the same material so pick the first faces
    %% material..
    [#e3d_face{mat=[Material|_]}|_] = Fs,
    %% Return the needed information {material, NoOfTriangles} and the binary
    {{Material,length(ListOfBinFs)},BinFs}.

%% element is 1 counted, e3d starts at 0..arrg..add one before looking up the object..
idx(Idx, Table) ->
   element(Idx+1, Table).

%% This converts a list of tuples of floats to a tuple of binaries.
%% It's much faster do element(Index,Table) than a list search operation.
convert_to_binary([{X,Y,Z}|Rest], Prev) ->
   Converted = <<X:32/float-little,Y:32/float-little,Z:32/float-little>>,
   convert_to_binary(Rest, [Converted|Prev]);
convert_to_binary([{U,V}|Rest], Prev) ->
   Converted = <<U:32/float-little,V:32/float-little>>,
   convert_to_binary(Rest, [Converted|Prev]);
convert_to_binary([],Done) ->
   Ordered = reverse(Done),
   list_to_tuple(Ordered).

%% With the first material
export_materials([{Name,Mat}|Mats],All) ->
    %% Get all info we need
    NameBin = name_to_binary(Name),
    OpMat = get_value(opengl, Mat),
    {DR,DG,DB,DA} = get_value(diffuse,OpMat),
    {AR,AG,AB,AA} = get_value(ambient,OpMat),
    {SR,SG,SB,SA} = get_value(specular,OpMat),
    {ER,EG,EB,EA} = get_value(emission,OpMat),
    Sh = get_value(shininess,OpMat),
    %% Get the texture images
    Maps = export_images(get_value(maps, Mat, []),[]), 
    %% Make the data for this material 
    MatBin = <<(size(NameBin)+1):32/little,NameBin/binary,0:8,
	      DR:32/float,DG:32/float,DB:32/float,DA:32/float,
	      AR:32/float,AG:32/float,AB:32/float,AA:32/float,
	      SR:32/float,SG:32/float,SB:32/float,SA:32/float,
	      ER:32/float,EG:32/float,EB:32/float,EA:32/float,
	      Sh:32/float, Maps/binary>>,
    %% Remember this material and handle the rest of the materials
    export_materials(Mats,[MatBin|All]);
%% We have transformed all materials, create the materialchunk.
export_materials([],All) ->
    list_to_binary(reverse(All)).

%% Export images loops through each texture image and creates a file
%% reference per image. The actual image is already exported to
%% separate files
export_images([],All) ->
    list_to_binary([<<(length(All)):32/little>>|All]);
export_images([{Type,#e3d_image{filename=File}}|Maps],All) ->
    TypeBin = name_to_binary(Type),
    FileBin = name_to_binary(File),
    %% The raw-image data is available in #e3d_image{}, so If you want
    %% the actual image to be included in the exported file, you could
    %% add it here instead of a file reference.
    ImageRef = <<(size(TypeBin)+1):32/little,TypeBin/binary, 0:8,
		(size(FileBin)+1):32/little,FileBin/binary, 0:8>>,
    export_images(Maps, [ImageRef|All]).

%% Make a object chunk per object
export_object_refs([{ObjName,ObjInfo}|Chunks], Start, Add) ->
    %% Collect the information
    {NextPos,Mats} = export_refs(ObjInfo,Start,[]),
    ObjNameBin = name_to_binary(ObjName),
    %% Create the binary
    ObjectRef = <<(size(ObjNameBin)+1):32/little,ObjNameBin/binary,0:8,
		 (length(Mats)):32/little,(list_to_binary(Mats))/binary>>,
    Size  = <<(size(ObjectRef)):32/little>>,
    %% Add the size in backwards order, the list is reverse in the last step.
    export_object_refs(Chunks, NextPos, [ObjectRef,Size|Add]);
export_object_refs([], _, ChunkList) ->
    list_to_binary(reverse(ChunkList)).

%% Foreach data chunk create a reference point: matName,startVertex,NoOfVertices
export_refs([{MatName,Triangles}|Next],Start,All) ->
    Mat = name_to_binary(MatName),
    Verts = Triangles*3,
    New = <<(size(Mat)+1):32/little,Mat/binary,0:8,Start:32/little,Verts:32/little>>,
    export_refs(Next, Start+Verts, [New|All]);
export_refs([],End,All) ->
    %% Remember to reverse the list so order is correct
    {End,lists:reverse(All)}.

%% A small help function.
name_to_binary(Name) when is_atom(Name) ->
    list_to_binary(atom_to_list(Name));
name_to_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
name_to_binary(Name) when is_binary(Name) ->
    Name.

