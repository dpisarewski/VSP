==Server==

Der Server kann mit dem Kommando server:start(). gestartet werden.
Er registriert sich mit dem in Konfigurationsdatei server.cfg angegebenen Namen.

Der Server kann durch die server.cfg Datei konfiguriert werden:
latency             Zeit in Sekunden, die der Server ungenutzt laufen kann
clientlifetime      Zeit in Sekunden, nach der ein Client vergossen wird
servername          Name des Serverprozesses
dlq_limit           Größe der DeliveryQueue
log_file            Dateiname für Logger-Ausgaben


==Client==
Um einen oder mehrere Clients zu starten, kann man folgendes Kommando benutzen client:start(serverNode).
Hier soll als Argument der Nodename übergeben werden, auf dem ein Server läuft.
Der Name des Serverprozesses lässt sich durch die Konfigurationsdatei client.cfg konfigurieren:

clients             Anzahl der zu startenden Clients
servername          Name des Serverprozesses
lifetime            Zeit in Sekunden, die ein Client lebt
sendeintervall      Zeitintervall in Sekunden zwischen Absenden von Nachrichten(als Redaktuer) an den Server
anzahl_nachrichten  Anzahl von Nachrichten, die der Redaktuer-Client absendet, bevor er zum Lese-Client wechselt
