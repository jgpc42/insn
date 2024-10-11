FROM openjdk:8

WORKDIR /root
COPY project.clj deps.edn /root/

RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x lein

ENV LEIN_ROOT=1
RUN ./lein deps
RUN ./lein with-profile +1.7 deps
RUN ./lein with-profile +1.9 deps
RUN ./lein with-profile +1.10 deps
RUN ./lein with-profile +1.11 deps
RUN ./lein with-profile +1.12 deps
