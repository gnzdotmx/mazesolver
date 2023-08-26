FROM ubuntu:22.04
RUN apt-get update

WORKDIR /app

COPY maze.cl ./
COPY *.maze ./

#Install common lisp
RUN apt-get install -y sbcl

#Execute the script
CMD ["sbcl","--load","maze.cl"]