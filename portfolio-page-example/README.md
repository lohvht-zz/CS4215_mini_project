# Instructions for compilation and running
1. Install elm dependencies `elm package install`
2. Compile elm code `elm make src/Main.elm --output src/Main.js`
3. Serve elm code via a http server of your choice, an easy choice would be `http-server`
   - Install `http-server` via `npm`: `npm install http-server -g`
   - run `http-server` to serve the webpage
   - Go to `localhost:8080/src` to see the page