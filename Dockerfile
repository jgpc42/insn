FROM openjdk:8

WORKDIR /root
COPY project.clj /root

RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN wget -O lein2.7.1 https://raw.githubusercontent.com/technomancy/leiningen/2.7.1/bin/lein
RUN chmod +x lein lein2.7.1

ENV LEIN_ROOT=1
RUN ./lein deps
RUN ./lein with-profile +1.7 deps
RUN ./lein with-profile +1.9 deps
RUN ./lein2.7.1 self-install
