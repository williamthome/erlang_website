-module(erlang_website_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).
-export([header/1]).
-export([erlang_logo/1]).
-export([footer/1]).
-export([footer_column/1]).

render(Bindings) ->
    Module = ?MODULE,
    Description = [
        ~"Erlang - Practical functional programming for a parallel world. ",
        ~"Build massively scalable soft real-time systems with fault tolerance."
    ],
    arizona_template:from_string(~"""
    <!DOCTYPE html>
    <html lang="en" class="h-full scroll-smooth">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="{Description}">
        <meta
            name="keywords"
            content="{[
                ~"Erlang, BEAM, OTP, functional programming, concurrent, ",
                ~"fault-tolerant, scalable, real-time"
            ]}"
        >
        <meta name="author" content="Erlang Programming Language">

        {% Open Graph / Facebook }
        <meta property="og:type" content="website">
        <meta property="og:title" content="{arizona_template:get_binding(title, Bindings)}">
        <meta property="og:description" content="{Description}">

        {% Twitter }
        <meta property="twitter:card" content="summary_large_image">
        <meta property="twitter:title" content="{arizona_template:get_binding(title, Bindings)}">
        <meta property="twitter:description" content="{Description}">

        <title>{arizona_template:get_binding(title, Bindings)}</title>
        <link rel="icon" type="image/x-icon" href="favicon.ico">
        <link rel="stylesheet" href="assets/app.css">
        <script type="module" src="assets/app.js" defer></script>
        <script src="assets/prism.js" defer></script>

        {% Modern fonts }
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link
            href="{[
                ~"https://fonts.googleapis.com/css2?",
                ~"family=Inter:wght@300;400;500;600;700&",
                ~"family=JetBrains+Mono:wght@300;400;500&display=swap"
            ]}"
            rel="stylesheet"
        >
    </head>
    <body class="bg-gray-950 text-gray-100 antialiased font-['Inter'] min-h-screen">
        {% Header Navigation }
        {arizona_template:render_stateless(Module, header, #{})}

        {% Main content }
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}

        {% Footer }
        {arizona_template:render_stateless(Module, footer, #{})}
    </body>
    </html>
    """).

header(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <header class="sticky top-0 z-50 bg-gray-950/95 backdrop-blur-sm border-b border-gray-800">
        <nav class="max-w-7xl mx-auto px-6 py-4">
            <div class="flex items-center justify-between">
                {% Logo }
                {arizona_template:render_stateless(Module, erlang_logo, #{})}

                {% Navigation Links }
                <div class="hidden xl:flex items-center space-x-6">
                    {arizona_template:render_list(fun(NavItem) ->
                        arizona_template:from_string(~"""
                        {arizona_template:render_stateless(erlang_website_components, link, NavItem#{
                            extra_classes => ~"text-gray-300 hover:text-erlang-red"
                        })}
                        """)
                    end, navigation_links())}
                </div>

                {% Search Input }
                <div class="hidden md:flex items-center lg:flex-1 lg:justify-end xl:flex-none xl:justify-start">
                    <div class="relative">
                        <input
                            type="search"
                            placeholder="Search erlang.org"
                            class="{[
                                ~"w-48 lg:w-56 xl:w-64 pl-4 pr-10 py-2 bg-gray-800 border border-gray-700 rounded-lg ",
                                ~"text-gray-300 placeholder-gray-500 focus:outline-none focus:ring-2 ",
                                ~"focus:ring-erlang-red focus:border-transparent transition-smooth"
                            ]}"
                        />
                        <button
                            type="submit"
                            class="{[
                                ~"absolute right-2 top-1/2 -translate-y-1/2 text-gray-400 ",
                                ~"hover:text-erlang-red transition-smooth"
                            ]}"
                        >
                            {arizona_template:render_stateless(erlang_website_components, icon, #{icon => search})}
                        </button>
                    </div>
                </div>

                {% Mobile Menu Button }
                {arizona_template:render_stateless(erlang_website_components, button, #{
                    id => ~"hamburger-button",
                    variant => secondary,
                    icon => bars,
                    extra_classes => ~"inline-flex xl:hidden"
                })}
            </div>
        </nav>
    </header>
    """").

erlang_logo(_Bindings) ->
    arizona_template:from_string(~"""
    <div class="flex items-center space-x-3">
        <div class="w-8 h-8 bg-erlang-red rounded-lg flex items-center justify-center">
            <span class="text-white font-bold text-3xl">e</span>
        </div>
        <span class="text-xl font-bold text-white">Erlang</span>
    </div>
    """).

footer(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <footer class="bg-gray-900 border-t border-gray-800 mt-16">
        <div class="max-w-7xl mx-auto px-6 py-12">
            <div class="grid md:grid-cols-4 gap-8">
                <div class="space-y-6">
                    {arizona_template:render_stateless(Module, erlang_logo, #{})}
                    <p class="text-gray-400 leading-relaxed">
                        Practical functional programming for a parallel world.
                    </p>
                </div>

                {arizona_template:render_list(fun(Column) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(Module, footer_column, Column)}
                    """)
                end, footer_columns())}
            </div>

            <div class="border-t border-gray-800 mt-8 pt-8 flex flex-col md:flex-row justify-between items-center">
                <p class="text-gray-400 text-sm text-center sm:text-left">
                    <span class="block sm:inline">© 2025 Erlang Programming Language</span>
                    <span class="hidden sm:inline mx-2">•</span>
                    <span class="block sm:inline">Built with Arizona Framework 🌵</span>
                </p>
                <div class="flex space-x-6 mt-4 md:mt-0">
                    {arizona_template:render_list(fun(Link) ->
                        arizona_template:from_string(~"""
                        {arizona_template:render_stateless(erlang_website_components, link, Link#{
                            extra_classes => ~"text-gray-400 hover:text-erlang-red"
                        })}
                        """)
                    end, social_media_links())}
                </div>
            </div>
        </div>
    </footer>
    """").

footer_column(Bindings) ->
    arizona_template:from_string(~""""
    <div>
        <h4 class="text-white font-semibold mb-4">
            {arizona_template:get_binding(title, Bindings)}
        </h4>
        <ul class="space-y-2 text-gray-400">
            {arizona_template:render_list(fun(Link) ->
                arizona_template:from_string(~"""
                <li>
                    {arizona_template:render_stateless(erlang_website_components, link, Link#{
                        extra_classes => ~"text-gray-300 hover:text-erlang-red"
                    })}
                </li>
                """)
            end, arizona_template:get_binding(links, Bindings))}
        </ul>
    </div>
    """").

% Internal functions

navigation_links() ->
    [
        #{label => ~"Download", href => ~"#"},
        #{label => ~"Documentation", href => ~"#"},
        #{label => ~"Community", href => ~"#"},
        #{label => ~"News", href => ~"#"},
        #{label => ~"Blog", href => ~"#"},
        #{label => ~"Security", href => ~"#"},
        #{label => ~"EEP", href => ~"#"},
        #{label => ~"About", href => ~"#"}
    ].

footer_columns() ->
    [
        #{
            title => ~"Learn",
            links => [
                #{label => ~"Getting Started", href => ~"#"},
                #{label => ~"Documentation", href => ~"#"},
                #{label => ~"Examples", href => ~"#"},
                #{label => ~"Books", href => ~"#"}
            ]
        },
        #{
            title => ~"Community",
            links => [
                #{label => ~"Forums", href => ~"#"},
                #{label => ~"Mailing Lists", href => ~"#"},
                #{label => ~"Slack", href => ~"#"},
                #{label => ~"Events", href => ~"#"}
            ]
        },
        #{
            title => ~"Project",
            links => [
                #{label => ~"Download", href => ~"#"},
                #{label => ~"Security", href => ~"#"},
                #{label => ~"EEP", href => ~"#"},
                #{label => ~"GitHub", href => ~"#"}
            ]
        }
    ].

social_media_links() ->
    [
        #{
            icon => github,
            href => ~"#"
        },
        #{
            icon => twitter,
            href => ~"#"
        }
    ].
