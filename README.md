Erlang Hotline client with a web UI.

Dependencies:
erlang: http://www.erlang.org/downloads
rebar3: https://www.rebar3.org/

1. Make a file in apps/hotline/priv/conf/local.conf that looks like this:

{connection_hostname, <<"example.com">>}.
{connection_title,    <<"Example">>}.
{connection_name,     <<"Nickname">>}.
{connection_username, <<"username">>}.
{connection_password, <<"password">>}.
{connection_icon,     150}.

2. Build with `make compile`

3. Run the app with `make start`

4. Point a browser at http://localhost:55500/

---

Wiki on the Hotline protocol: http://hotline.wikia.com/wiki/Hotline_Wiki
