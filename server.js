const express = require('express');
const app = express();

app.use(express.static('./'));
app.use(express.static('./assets'));

app.get('*', (req, res) => {
  res.sendFile(__dirname + '/index.html');
});

const server = app.listen(3005, () => {
  const host = server.address().address;
  const port = server.address().port;

  console.log('Elm list is listening at http://%s:%s', host, port);
});
