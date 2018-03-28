# BUDE Alanine Scan

## Building and Running

A few steps are required to run the application:

1. Add the ISAMBARD dependencies to the `dependencies_for_isambard/` folder.
   SCWRL is the most vital of these.
2. Add an `nginx.conf` file to `web/instance/`. Instance contains all the
   potentially sensitive files (secret keys etc) in the application, which
   **SHOULD NOT BE ADDED TO VERSION CONTROL!**
3. `cd` to `web/elm-src/` and type `elm make Main.elm
   --output=../static/elm/bals.js --debug`. Remove the debug flag if the app is
   being used in production. _This step requires the Elm toolchain to be
   installed._
4. Run `docker-compose up --build`.

Once these steps have been taken the application should be available on
`0.0.0.0:3803`. If modifications are made to the Elm application recompile the
Elm code and fully refresh your browser (`ctrl+shift+r` generally). If you make
changes to the back-end code restart the Docker containers.

Good luck, have fun and feel free to report any issues on the GitHub page.
