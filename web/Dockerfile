FROM tiangolo/uwsgi-nginx-flask:python3.6
# Setup BALS
WORKDIR /app
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./web/requirements.txt ./
RUN pip install cython
RUN pip install -r ./requirements.txt
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/
# RUN rm /etc/nginx/conf.d/nginx.conf
COPY ./web/config/nginx.conf /etc/nginx/nginx.conf
COPY ./web/config/bals.conf /etc/nginx/conf.d/
COPY ./web/ /app/
WORKDIR /app/elm-src
RUN elm make Main.elm --optimize --output=../static/elm/bals.js
