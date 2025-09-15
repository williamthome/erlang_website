-module(erlang_website_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).
-export([header/1]).
-export([nav_link/1]).
-export([logo/1]).
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
        <meta name="keywords" content="Erlang, BEAM, OTP, functional programming, concurrent, fault-tolerant, scalable, real-time">
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

        {% Modern fonts }
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@300;400;500&display=swap" rel="stylesheet">
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
                {arizona_template:render_stateless(Module, logo, #{})}

                {% Navigation Links }
                <div class="hidden md:flex items-center space-x-8">
                    {arizona_template:render_list(fun(NavItem) ->
                        arizona_template:from_string(~"""
                        {arizona_template:render_stateless(Module, nav_link, NavItem)}
                        """)
                    end, [
                        #{label => ~"Download", href => ~"#"},
                        #{label => ~"Documentation", href => ~"#"},
                        #{label => ~"Community", href => ~"#"},
                        #{label => ~"News", href => ~"#"},
                        #{label => ~"Blog", href => ~"#"},
                        #{label => ~"Security", href => ~"#"},
                        #{label => ~"EEP", href => ~"#"},
                        #{label => ~"About", href => ~"#"}
                    ])}
                </div>

                {% Mobile Menu Button }
                <button class="md:hidden text-gray-300 hover:text-white">
                    <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 6h16M4 12h16M4 18h16"></path>
                    </svg>
                </button>
            </div>
        </nav>
    </header>
    """").

logo(_Bindings) ->
    arizona_template:from_string(~"""
    <div class="flex items-center space-x-3">
        <div class="w-8 h-8 bg-erlang-red rounded-lg flex items-center justify-center">
            <span class="text-white font-bold text-3xl">e</span>
        </div>
        <span class="text-xl font-bold text-white">Erlang</span>
    </div>
    """).

nav_link(Bindings) ->
    arizona_template:from_string(~"""
    <a href="{arizona_template:get_binding(href, Bindings)}" class="text-gray-300 hover:text-erlang-red transition-smooth">
        {arizona_template:get_binding(label, Bindings)}
    </a>
    """).

footer(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <footer class="bg-gray-900 border-t border-gray-800 mt-16">
        <div class="max-w-7xl mx-auto px-6 py-12">
            <div class="grid md:grid-cols-4 gap-8">
                <div class="space-y-6">
                    {% Logo }
                    {arizona_template:render_stateless(Module, logo, #{})}

                    <p class="text-gray-400 leading-relaxed">
                        Practical functional programming for a parallel world.
                    </p>
                </div>

                {arizona_template:render_list(fun(Column) ->
                    arizona_template:from_string(~"""
                    {arizona_template:render_stateless(Module, footer_column, Column)}
                    """)
                end, [
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
                ])}
            </div>

            <div class="border-t border-gray-800 mt-8 pt-8 flex flex-col md:flex-row justify-between items-center">
                <p class="text-gray-400 text-sm text-center sm:text-left">
                    <span class="block sm:inline">© 2025 Erlang Programming Language</span>
                    <span class="hidden sm:inline mx-2">•</span>
                    <span class="block sm:inline">Built with Arizona Framework 🌵</span>
                </p>
                <div class="flex space-x-6 mt-4 md:mt-0">
                    <a href="#" class="text-gray-400 hover:text-erlang-red transition-smooth">
                        <span class="sr-only">GitHub</span>
                        <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M12 0C5.374 0 0 5.373 0 12 0 17.302 3.438 21.8 8.207 23.387c.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23A11.509 11.509 0 0112 5.803c1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.30 3.297-1.30.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576C20.566 21.797 24 17.3 24 12c0-6.627-5.373-12-12-12z"/>
                        </svg>
                    </a>
                    <a href="#" class="text-gray-400 hover:text-erlang-red transition-smooth">
                        <span class="sr-only">Twitter</span>
                        <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                            <path d="M23.953 4.57a10 10 0 01-2.825.775 4.958 4.958 0 002.163-2.723c-.951.555-2.005.959-3.127 1.184a4.92 4.92 0 00-8.384 4.482C7.69 8.095 4.067 6.13 1.64 3.162a4.822 4.822 0 00-.666 2.475c0 1.71.87 3.213 2.188 4.096a4.904 4.904 0 01-2.228-.616v.06a4.923 4.923 0 003.946 4.827 4.996 4.996 0 01-2.212.085 4.936 4.936 0 004.604 3.417 9.867 9.867 0 01-6.102 2.105c-.39 0-.779-.023-1.17-.067a13.995 13.995 0 007.557 2.209c9.053 0 13.998-7.496 13.998-13.985 0-.21 0-.42-.015-.63A9.935 9.935 0 0024 4.59z"/>
                        </svg>
                    </a>
                </div>
            </div>
        </div>
    </footer>
    """").

footer_column(Bindings) ->
    arizona_template:from_string(~""""
    <div>
        <h4 class="text-white font-semibold mb-4">{arizona_template:get_binding(title, Bindings)}</h4>
        <ul class="space-y-2 text-gray-400">
            {arizona_template:render_list(fun(Link) ->
                arizona_template:from_string(~"""
                <li>
                    <a href="{arizona_template:get_binding(href, Link)}" class="hover:text-erlang-red transition-smooth">
                        {arizona_template:get_binding(label, Link)}
                    </a>
                </li>
                """)
            end, arizona_template:get_binding(links, Bindings))}
        </ul>
    </div>
    """").
