FROM tiangolo/uwsgi-nginx-flask:flask-python3.5
# Setup nodejs
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get update
RUN apt-get install -y nodejs
# Setup Elm
RUN npm install -g elm --unsafe-perm=true --allow-root
RUN npm install -g elm-css
# Setup pandoc
RUN wget https://github.com/jgm/pandoc/releases/download/1.19.1/pandoc-1.19.1-1-amd64.deb
RUN dpkg -i pandoc-1.19.1-1-amd64.deb
# Setup ISAMBARD
RUN git clone https://github.com/woolfson-group/isambard.git
WORKDIR isambard
RUN pip install -r ./requirements.txt
RUN python setup.py install
# Setup BALS
WORKDIR /app
# COPY ./web/requirements.txt ./
# RUN pip install -r ./requirements.txt
COPY ./dependencies_for_isambard/.isambard_settings /root/
# RUN elm-css ./ccbmk2/elm-src/css/Stylesheets.elm --output=/app/static/css/
# RUN elm-make ./ccbmk2/elm-src/Main.elm --output=/app/static/elm/builder.js
COPY ./web/ /app/
