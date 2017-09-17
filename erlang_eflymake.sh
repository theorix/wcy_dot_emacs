#!/usr/bin/env escript
-export([main/1]).

main([File_Name]) ->

    {ok, Cwd} = file:get_cwd(),
    %%file:write_file("/home/zjh/d/working/1.txt",io_lib:format("~p",[Cwd])),
    BaseDir0 =
    case re:run(Cwd, "^(.+)(/src|/test).?", [{capture, [1], list}]) of
    {match, [B]} ->
	    B ++ "/";
    nomatch ->
	    Cwd ++ "/"
    end,
    BaseDir = case Cwd of
                  "/home/zou/d/working/msync"++_ ->
                      "/home/zou/d/working/msync/";
                  _ ->
                      "/home/zou/d/working/ejabberd/"
              end,
    case file:list_dir(BaseDir ++ "deps") of
        {ok, Deps} -> ok;
        _ -> Deps = []
    end,
    case file:list_dir(BaseDir ++ "src") of
        {ok, SrcDirs} -> ok;
        _ -> SrcDirs = []
    end,

    Includes =
	[{i, BaseDir ++ "include"}, {i, BaseDir ++ "deps"}, {i, BaseDir ++ "src"},{i,BaseDir ++ "deps/im_libs/apps/message_store/include"},{i, BaseDir},{i, BaseDir++"tools"}]
	++
	[{i, BaseDir ++ "deps/im_libs/apps/msync_proto/include"}]
    ++
    [{i, BaseDir ++ "deps/brod/include"}]
    ++ 
    [{i, BaseDir ++ "deps/kafka_protocol/include"}]
    ++
    [{i, BaseDir ++ "deps/brod/src"}]
	++

        [{i, Si} || S <- SrcDirs,
		    begin
                                  Si = BaseDir ++ "src/" ++ S,
                                  filelib:is_dir(Si)

		    end]

	++

        [{i,Di} || D <- Deps,
                              begin
                                  Di = BaseDir ++ "deps/"++D++"/include",
                                  filelib:is_dir(Di)
                              end],
    %io:format("I:~p",[Includes]),
    file:write_file("/home/zou/d/working/1.txt",io_lib:format("BaseDir=~p~nCwd=~p,~nFileName=~p~n,Includes=~p~n",[BaseDir,Cwd, File_Name, Includes])),
    [code:add_patha(Di)||D <- Deps,
                              begin
                                  Di = BaseDir ++ "deps/"++D++"/ebin",
                                  filelib:is_dir(Di)
                              end],
    code:add_patha(BaseDir++"ebin"),
    code:add_patha(BaseDir),
    compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
			     %warnings_as_errors,
                             warn_shadow_vars, warn_export_vars,
                             strong_validation, report | Includes]).
