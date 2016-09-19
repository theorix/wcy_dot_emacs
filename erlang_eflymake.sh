#!/usr/bin/env escript
-export([main/1]).

main([File_Name]) ->

    {ok, Cwd} = file:get_cwd(),
    %%file:write_file("/home/zjh/d/working/1.txt",io_lib:format("~p",[Cwd])),
    BaseDir =
    case re:run(Cwd, "^(.+)(/src).?", [{capture, [1], list}]) of
    {match, [B]} ->
	    B ++ "/";
    nomatch ->
	    Cwd ++ "/"
    end,

    {ok, Deps} = file:list_dir(BaseDir ++ "deps"),
    {ok, SrcDirs} = file:list_dir(BaseDir ++ "src"),

    Includes =
	[{i, BaseDir ++ "include"}, {i, BaseDir ++ "deps"}, {i, BaseDir ++ "src"},{i,BaseDir ++ "deps/im_libs/apps/message_store/include"}]

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
    %file:write_file("/home/zjh/d/working/1.txt",io_lib:format("BaseDir=~p~nCwd=~p,~nFileName=~p~n",[BaseDir,Cwd, File_Name])),
    [code:add_patha(Di)||D <- Deps,
                              begin
                                  Di = BaseDir ++ "deps/"++D++"/ebin",
                                  filelib:is_dir(Di)
                              end],
    code:add_patha(BaseDir++"ebin"),
    compile:file(File_Name, [warn_obsolete_guard, warn_unused_import,
			     %warnings_as_errors,
                             warn_shadow_vars, warn_export_vars,
                             strong_validation, report | Includes]).
