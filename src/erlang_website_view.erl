-module(erlang_website_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
% Components
-export([header/1]).
-export([nav_link/1]).
-export([hero/1]).
-export([hero_button/1]).
-export([code_showcase/1]).
-export([what_is_erlang/1]).
-export([what_is_otp/1]).
-export([news_section/1]).
-export([news_article/1]).
-export([participate_section/1]).
-export([footer/1]).
-export([footer_column/1]).
-export([cta_link/1]).

mount(#{title := Title}, _Req) ->
    Bindings = #{id => ~"home"},
    Layout =
        {erlang_website_layout, render, main_content, #{
            title => Title
        }},
    arizona_view:new(?MODULE, Bindings, Layout).

render(Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="min-h-screen">
        {% Header Navigation }
        {arizona_template:render_stateless(Module, header, #{})}

        {% Hero Section }
        {arizona_template:render_stateless(Module, hero, #{})}

        {% Main Content Grid }
        <main class="max-w-7xl mx-auto px-6 py-16 grid lg:grid-cols-2 gap-16">
            {% Left Column }
            <div class="space-y-16">
                {arizona_template:render_stateless(Module, news_section, #{})}
            </div>

            {% Right Column }
            <div class="space-y-16">
                {arizona_template:render_stateless(Module, what_is_erlang, #{})}
                {arizona_template:render_stateless(Module, what_is_otp, #{})}
                {arizona_template:render_stateless(Module, participate_section, #{})}
            </div>
        </main>

        {% Footer }
        {arizona_template:render_stateless(Module, footer, #{})}
    </div>
    """).

header(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <header class="sticky top-0 z-50 bg-gray-950/95 backdrop-blur-sm border-b border-gray-800">
        <nav class="max-w-7xl mx-auto px-6 py-4">
            <div class="flex items-center justify-between">
                {% Logo }
                <div class="flex items-center space-x-3">
                    <div class="w-8 h-8 bg-erlang-red rounded-lg flex items-center justify-center">
                        <span class="text-white font-bold text-3xl">e</span>
                    </div>
                    <span class="text-xl font-bold text-white">Erlang</span>
                </div>

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

nav_link(Bindings) ->
    arizona_template:from_string(~"""
    <a href="{arizona_template:get_binding(href, Bindings)}" class="text-gray-300 hover:text-erlang-red transition-smooth">
        {arizona_template:get_binding(label, Bindings)}
    </a>
    """).

hero_button(Bindings) ->
    arizona_template:from_string(~""""
    <a
        href="{arizona_template:get_binding(href, Bindings)}"
        class="{case arizona_template:get_binding(variant, Bindings) of
            ~"primary" -> ~"inline-flex items-center px-8 py-4 bg-erlang-red hover:bg-erlang-red-dark text-white font-semibold rounded-lg transition-smooth";
            ~"secondary" -> ~"inline-flex items-center px-8 py-4 border-2 border-gray-600 hover:border-erlang-red text-gray-300 hover:text-white font-semibold rounded-lg transition-smooth";
            _ -> ~"inline-flex items-center px-8 py-4 bg-gray-600 text-white font-semibold rounded-lg transition-smooth"
        end}"
    >
        {arizona_template:get_binding(label, Bindings)}
        {case arizona_template:get_binding(icon, Bindings) of
            ~"download" -> arizona_template:from_string(~"""
                <svg class="ml-2 w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4 4m0 0l-4-4m4 4V4"></path>
                </svg>
                """);
            ~"none" -> ~"";
            _ -> ~""
        end}
    </a>
    """").

hero(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <section class="relative bg-gradient-to-br from-gray-950 via-gray-900 to-gray-950 overflow-hidden">
        {% Background Pattern }
        <div class="absolute inset-0 bg-grid-pattern opacity-5"></div>

        <div class="max-w-7xl mx-auto px-4 sm:px-6 py-16 sm:py-24 lg:py-32">
            <div class="grid lg:grid-cols-2 gap-16 items-center">
                {% Left Column - Content }
                <div class="text-center lg:text-left">
                    <h1 class="text-3xl sm:text-4xl md:text-5xl lg:text-7xl font-bold text-white mb-6 sm:mb-8 leading-tight">
                        Practical
                        <span class="text-erlang-red">functional</span>
                        programming
                        <span class="block text-2xl sm:text-3xl md:text-4xl lg:text-5xl text-gray-300 mt-2 sm:mt-4">
                            for a parallel world
                        </span>
                    </h1>

                    <p class="text-lg sm:text-xl text-gray-300 mb-8 sm:mb-10 leading-relaxed max-w-2xl px-4 sm:px-0">
                        Build massively scalable soft real-time systems with requirements on
                        high availability using Erlang's fault-tolerant design.
                    </p>

                    <div class="flex flex-col sm:flex-row gap-4">
                        {arizona_template:render_list(fun(Button) ->
                            arizona_template:from_string(~"""
                            {arizona_template:render_stateless(Module, hero_button, Button)}
                            """)
                        end, [
                            #{
                                label => ~"Get Erlang/OTP 28",
                                href => ~"#",
                                variant => ~"primary",
                                icon => ~"download"
                            },
                            #{
                                label => ~"Learn More",
                                href => ~"#",
                                variant => ~"secondary",
                                icon => ~"none"
                            }
                        ])}
                    </div>
                </div>

                {% Right Column - Code Showcase }
                <div class="relative overflow-hidden">
                    {arizona_template:render_stateless(Module, code_showcase, #{})}
                </div>
            </div>
        </div>
    </section>
    """").

code_showcase(_Bindings) ->
    arizona_template:from_string(~"""
    <div class="relative bg-gray-900 rounded-2xl border border-gray-700 overflow-hidden shadow-2xl">
        {% Terminal Header }
        <div class="flex items-center px-6 py-4 bg-gray-800 border-b border-gray-700">
            <div class="flex space-x-2">
                <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                <div class="w-3 h-3 bg-green-500 rounded-full"></div>
            </div>
            <div class="flex-1 text-center text-gray-400 text-sm font-mono">factorial.erl</div>
        </div>

        {% Code Content }
        <div class="p-3 sm:p-6 overflow-x-auto">
            <pre class="text-xs sm:text-sm leading-relaxed"><code class="language-erlang whitespace-pre"><span class="text-gray-400">%% Pattern matching for control-flow</span>
    <span class="text-erlang-blue">fact</span>(<span class="text-yellow-400">0</span>) -> <span class="text-yellow-400">1</span>;
    <span class="text-gray-400">%% Interactive shell for fast iterations</span>
    <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">N</span>) -> <span class="text-blue-400">N</span> * <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">N</span>-<span class="text-yellow-400">1</span>).

    <span class="text-gray-400">%% Recursion to create loops</span>
    <span class="text-gray-500">></span> <span class="text-erlang-blue">fact</span>(<span class="text-yellow-400">10</span>).
    <span class="text-yellow-400">3628800</span>
    <span class="text-gray-500">></span> [\{<span class="text-blue-400">I</span>, <span class="text-erlang-blue">fact</span>(<span class="text-blue-400">I</span>)} || <span class="text-blue-400">I</span> <- <span class="text-erlang-blue">lists:seq</span>(<span class="text-yellow-400">1</span>,<span class="text-yellow-400">10</span>)].
    <span class="text-gray-300">[\{1, 1}, \{2, 2}, \{3, 6}, \{4, 24}, \{5, 120}, \{6, 720},
     \{7, 5040}, \{8, 40320}, \{9, 362880}, \{10, 3628800}]</span></code></pre>
        </div>

        {% Label }
        <div class="px-3 sm:px-6 mb-4 sm:mb-6">
            <div class="flex justify-center">
                <div class="bg-erlang-red px-2 sm:px-3 py-1 rounded-full text-white text-xs font-semibold">
                    Functional programming
                </div>
            </div>
        </div>
    </div>
    """).

what_is_erlang(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">What is Erlang?</h2>
        <div class="space-y-4 text-gray-300 leading-relaxed">
            <p>
                Erlang is a programming language used to build massively scalable soft
                real-time systems with requirements on <strong class="text-erlang-red">high availability</strong>.
                Some of its uses are in telecoms, banking, e-commerce, computer telephony and instant messaging.
            </p>
            <p>
                Erlang's runtime system has built-in support for <strong class="text-erlang-red">concurrency</strong>,
                <strong class="text-erlang-red">distribution</strong> and <strong class="text-erlang-red">fault tolerance</strong>.
            </p>
            <div class="pt-4">
                {arizona_template:render_stateless(Module, cta_link, #{
                    label => ~"Erlang Quickstart",
                    href => ~"#"
                })}
            </div>
        </div>
    </section>
    """).

what_is_otp(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">What is OTP?</h2>
        <div class="space-y-4 text-gray-300 leading-relaxed">
            <p>
                OTP is set of Erlang libraries and design principles providing middle-ware to
                develop these systems. It includes its own distributed database, applications
                to interface towards other languages, debugging and release handling tools.
            </p>
            <div class="pt-4">
                {arizona_template:render_stateless(Module, cta_link, #{
                    label => ~"Getting Started with OTP",
                    href => ~"#"
                })}
            </div>
        </div>
    </section>
    """).

news_section(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <section>
        <h2 class="text-3xl font-bold text-white mb-8">News</h2>
        <div class="space-y-6">
            {arizona_template:render_list(fun(Article) ->
                arizona_template:from_string(~"""
                {arizona_template:render_stateless(Module, news_article, Article)}
                """)
            end, [
                #{
                    title => ~"Erlang/OTP 28.0",
                    href => ~"#",
                    date => ~"May 21, 2025",
                    author => ~"Henrik Nord",
                    summary => ~"Erlang/OTP 28.0 is a new major release with new features, improvements as well as a few incompatibilities."
                },
                #{
                    title => ~"Erlang/OTP 28 Highlights",
                    href => ~"#",
                    date => ~"May 20, 2025",
                    author => ~"Isabell Huang",
                    summary => ~"Erlang/OTP 28 is finally here. This blog post will introduce the new features that we are most excited about."
                },
                #{
                    title => ~"Erlang/OTP 28.0 Release Candidate 4",
                    href => ~"#",
                    date => ~"May 05, 2025",
                    author => ~"Björn Gustavsson",
                    summary => ~"Erlang/OTP 28.0-rc4 is the fourth release candidate for OTP 28."
                }
            ])}
        </div>
    </section>
    """").

news_article(Bindings) ->
    arizona_template:from_string(~"""
    <article class="bg-gray-900 rounded-2xl p-6 border border-gray-700 hover:border-gray-600 transition-smooth">
        <h3 class="text-xl font-semibold text-erlang-red mb-2">
            <a href="{arizona_template:get_binding(href, Bindings)}" class="hover:text-erlang-red-light transition-smooth">
                {arizona_template:get_binding(title, Bindings)}
            </a>
        </h3>
        <p class="text-gray-400 text-sm mb-3">
            {arizona_template:get_binding(date, Bindings)} by {arizona_template:get_binding(author, Bindings)}
        </p>
        <p class="text-gray-300 leading-relaxed">
            {arizona_template:get_binding(summary, Bindings)}
        </p>
    </article>
    """).

participate_section(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~"""
    <section class="bg-gray-900 rounded-2xl p-8 border border-gray-700">
        <h2 class="text-3xl font-bold text-white mb-6">Participate</h2>
        <div class="text-center">
            <div class="inline-flex items-center justify-center w-24 h-24 bg-white rounded-full mb-6">
                <img src="images/eef-logo.svg" alt="Erlang Ecosystem Foundation" class="w-16 h-16" />
            </div>
            <p class="text-gray-300 leading-relaxed mb-6">
                Join the Erlang Ecosystem Foundation and be part of the community
                that builds the future of concurrent, fault-tolerant systems.
            </p>
            {arizona_template:render_stateless(Module, hero_button, #{
                label => ~"Join the Community",
                href => ~"#",
                variant => ~"primary",
                icon => ~"none"
            })}
        </div>
    </section>
    """).

footer(_Bindings) ->
    Module = ?MODULE,
    arizona_template:from_string(~""""
    <footer class="bg-gray-900 border-t border-gray-800 mt-16">
        <div class="max-w-7xl mx-auto px-6 py-12">
            <div class="grid md:grid-cols-4 gap-8">
                <div>
                    <div class="flex items-center space-x-3 mb-6">
                        <div class="w-8 h-8 bg-erlang-red rounded-lg flex items-center justify-center">
                            <span class="text-white font-bold text-lg">E</span>
                        </div>
                        <span class="text-xl font-bold text-white">Erlang</span>
                    </div>
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

cta_link(Bindings) ->
    arizona_template:from_string(~"""
    <a href="{arizona_template:get_binding(href, Bindings)}" class="inline-flex items-center text-erlang-red hover:text-erlang-red-light transition-smooth">
        {arizona_template:get_binding(label, Bindings)}
        <svg class="ml-2 w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"></path>
        </svg>
    </a>
    """).
