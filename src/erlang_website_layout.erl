-module(erlang_website_layout).
-compile({parse_transform, arizona_parse_transform}).
-export([render/1]).

render(Bindings) ->
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
        <script type="module" async>
            import Arizona from '@arizona-framework/client';
            globalThis.arizona = new Arizona(\{ logLevel: 'debug' });
            arizona.connect(\{ wsPath: '/live' });
        </script>
        <script type="module" src="assets/app.js" defer></script>

        {% Modern fonts }
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@300;400;500&display=swap" rel="stylesheet">
    </head>
    <body class="bg-gray-950 text-gray-100 antialiased font-['Inter'] min-h-screen">
        {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
    </body>
    </html>
    """).
