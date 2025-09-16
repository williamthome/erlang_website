-module(erlang_website_static).
-export([generate/0]).

generate() ->
    ok = application:set_env(erlang_website, env, prod),
    {ok, Cwd} = file:get_cwd(),
    OutputDir = filename:join([Cwd, "dist"]),
    ok =
        case file:del_dir_r(OutputDir) of
            ok -> ok;
            {error, enoent} -> ok
        end,

    % Configuration for static site generation
    Config = #{
        base_url => ~"https://erlang.org",
        route_paths => #{
            ~"/" => #{parallel => true},
            ~"/favicon.ico" => #{parallel => true},
            ~"/robots.txt" => #{parallel => true},
            ~"/assets/app.js" => #{parallel => true},
            ~"/assets/prism.js" => #{parallel => true},
            ~"/assets/app.css" => #{parallel => true},
            ~"/images/eef-logo.svg" => #{parallel => true}
        },
        output_dir => OutputDir
    },

    % Generate the static site
    Result =
        case arizona_static:generate(Config) of
            ok ->
                io:format("Static site generated successfully in '~s' directory!~n", [OutputDir]),
                ok;
            {error, Reason} ->
                io:format("Failed to generate static site: ~p~n", [Reason]),
                {error, Reason}
        end,
    ok = application:set_env(erlang_website, env, dev),
    Result.
