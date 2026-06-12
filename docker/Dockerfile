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
    
# Install Rust for Ascent and DDLog
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup component add rustfmt clippy

# Install z3
#RUN apt-get update && apt-get install -y z3

# Update sbt
RUN sbt sbtVersion && \
 	sbt update
 	
# Install and publish sturdy locally
#RUN git clone https://gitlab.rlp.net/plmz/sturdy.scala.git
#RUN cd sturdy.scala && sbt sturdy_core/publishLocal

# Install and publish scala-benchmark locally
#RUN git clone https://gitlab.rlp.net/plmz/benchmark-scala.git
#RUN cd benchmark-scala && sbt publishLocal

# Install DDLog
ARG DDLOG_REPOSITORY=https://github.com/Schlaubischlump/DDlog.git
ARG DDLOG_REF=master
ENV DDLOG_HOME=/opt/ddlog
ENV CLASSPATH="${DDLOG_HOME}/flatbuffers/java"
ENV PATH="${DDLOG_HOME}/flatbuffers:${PATH}"

RUN apt-get update -y && \
	apt-get install -y --no-install-recommends \
	autoconf \
	build-essential \
	cmake \
	git \
	gnupg \
	libc6-dev \
	libffi-dev \
	libgmp-dev \
	libncurses-dev \
	libssl-dev \
	libtinfo-dev \
	libtool \
	make \
	pkg-config \
	subversion \
	unzip \
	zlib1g-dev && \
	rm -rf /var/lib/apt/lists/*

RUN wget -qO- https://get.haskellstack.org/ | sh

RUN git clone "${DDLOG_REPOSITORY}" "${DDLOG_HOME}" && \
	cd "${DDLOG_HOME}" && \
	git checkout "${DDLOG_REF}" && \
	./tools/install-flatbuf.sh && \
	stack --install-ghc build && \
	stack install --local-bin-path /usr/local/bin && \
	ddlog --version
