%% Serveur de jeu pour RPG MAKER
%% 2011 - Dépend de la libriarie RMVXConnecteur.dll
%% Auteurs: Xavier Van de Woestyne & Maxime Legrand
%% --------------------------------------------------------------------------
%% Ce Module permet de gerer des entrée et des sorties du serveur
%% ainsi que d'envoyer les informations de l'adverssaire via le protocol TCP/IP
%% Pour fonctionner, le client requiert RMVXConnecteur.dll ainsi qu'une carte
%% admettant le mode Multi-joueur et respectant les standards de multi-joueur
%% prévu.
%% ---------------------------------------------------------------------------

-module(rmvx_serveur).
-export([start/0, start/1, stop/1, server/1, client_register/1]).
-export([extract_data/1, query_list/2]).
%% Maccros
-define(default_server, localhost).
-define(default_port, 9999).

%% Procédure de démarrage du serveur avec le port par défaut.
start() ->
   start(?default_port).

%% Procédure du lancement du serveur avec un port spécifique.
start(Port) ->
   Ecouteur = case gen_tcp:listen(Port, [{packet, 0}, {active, false}, {reuseaddr, true}]) of 
      %% Connexion au serveur possible
      {ok, EcouteurSocket} -> 
         %% Initialisation de la base de donnée pour le Salon de jeu
         io:format("Serveur démarré sur le port: ~p~n", [Port]),
         EcouteurSocket;
      %% Connexion au serveur Impossible
      {error, Raison} ->
         io:format("Serveur impossible à démarrer sur le port ~p: [~p]~n", [Port, Raison]),
         exit(Raison)
      end,
   spawn(?MODULE, server, [Ecouteur]).

%% Procédure d'arrêt du serveur
stop(Pid) ->
   Pid ! {stop},
   ok.

%% Procédure mis en processus du serveur
server(EcouteurSocket) ->
   %% Autorise la continuité du processus malgré une déconnexion
   process_flag(trap_exit, true),
   boucle_server(EcouteurSocket).

%% Bouclage du serveur 
boucle_server(EcouteurSocket) ->
   %% Receptions des messages Admin du serveur
   receive
      {stop} ->
         io:format("Serveur arrêté~n", []),
         exit(normal);
      {'EXIT', Pid, Raison} ->
         %% Destruction de la session Mnesia du joueur
         io:format("Un client vient de se déconnecter~n", [])
      after 100 -> timeout 
   end,
   
   %% Traitement des demandes de connexion au serveur
   case gen_tcp:accept(EcouteurSocket, 100) of
      {ok, Socket} ->
         %% Ici que sera défini la création du salon de jeu
         io:format("Connexion d'un nouveau client~n", []),
         PClientID = spawn_link(?MODULE, client_register, [Socket]),
         gen_tcp:controlling_process(Socket, PClientID),
         server(EcouteurSocket);
      %% Timer de repos au serveur
      {error, timeout} ->
         server(EcouteurSocket)
   end.

%% Gestionnaire des clients
client_register(Socket) ->
   case gen_tcp:recv(Socket, 0) of
      {ok, Donnees} ->
         %% Ici que la gestion des sauvegarde aux bases de données seront effectuées UPDATE
         AList = extract_data(Donnees),
         io:format("Position d'un joueur AT -> ~p~n", [AList]),
         client_register(Socket);
      {error, closed} ->
         gen_tcp:close(Socket)
   end.
   
%% Manipulation de la liste recue via RMVXConnecteur.dll
extract_data(List) ->
   ListFinal = query_list(List, []),
   ListFinal.
%% Traitement et isolation des coordonnées
query_list([], List) -> List;
query_list([59|T], List) -> 
   AList = lists:append(List, [45]),
   query_list(T, AList);
query_list([10|T], List) -> List;
query_list([H|T], []) -> 
   query_list(T, [H]);
query_list([H|T], List) ->
   AList = lists:append(List, [H]),
   query_list(T, AList).
   
%% Fin du module rmvx_serveur
