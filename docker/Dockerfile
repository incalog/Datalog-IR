# Build docker-image for intel on mX macs: docker build --platform linux/amd64 -t sbt-image .
FROM sbtscala/scala-sbt:eclipse-temurin-focal-17.0.9_9_1.9.8_3.3.1

SHELL ["/bin/bash", "-c"]

RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/*

# Install Souffle
RUN wget https://souffle-lang.github.io/ppa/souffle-key.public -O /usr/share/keyrings/souffle-archive-keyring.gpg && \
	echo "deb [signed-by=/usr/share/keyrings/souffle-archive-keyring.gpg] https://souffle-lang.github.io/ppa/ubuntu/ stable main" | tee /etc/apt/sources.list.d/souffle.list && \
	apt-get update -y && \
	apt-get install -y souffle && \
    rm -rf /var/lib/apt/lists/*
    
# Install Rust for Ascent
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Install z3
RUN apt-get update && apt-get install -y z3

# Update sbt
RUN sbt sbtVersion && \
 	sbt update
 	
# Install and publish sturdy locally
RUN git clone https://gitlab.rlp.net/plmz/sturdy.scala.git
RUN cd sturdy.scala && sbt sturdy_core/publishLocal

# Install DDLog
#RUN apt-get update -y && \ 
#	apt-get install -y make gnupg build-essential libtinfo-dev zlib1g-dev libgmp-dev libc6-dev libgmp-dev
#RUN wget -qO- https://get.haskellstack.org/ | sh
#RUN git clone https://github.com/BonoboBoss/DDlog
#RUN export DDLOG_HOME="$HOME/DDlog" && cd DDlog && stack build && stack install