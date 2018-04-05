/*
Habiendo creado una base de datos llamada friends

1.
Generate 'Facebook' table where every person is identified with the name, age, city of residence, and email. The table should at least have 10 rows.

*/

CREATE TABLE Facebook (nombre VARCHAR, edad INT, ciudad_residencia VARCHAR, email VARCHAR);

-- Añadimos valores a la tabla

INSERT INTO Facebook VALUES ('José María', 38, 'Donostia', 'jose@ferrer.com');
INSERT INTO Facebook VALUES ('Ignacio', 37, 'Pamplona', 'ignacio@ferrer.com');
INSERT INTO Facebook VALUES ('Alejandro', 35, 'Pamplona', 'alejandro@ferrer.com');
INSERT INTO Facebook VALUES ('Eduardo', 34, 'León', 'eduardo@ferrer.com');
INSERT INTO Facebook VALUES ('Javier', 33, 'Bilbao', 'javi@ferrer.com');
INSERT INTO Facebook VALUES ('María', 27, 'Sevilla', 'maria@ferrer.com');
INSERT INTO Facebook VALUES ('Álvaro', 25, 'Madrid', 'alvaro@ferrer.com');
INSERT INTO Facebook VALUES ('Juan', 23, 'Donostia', 'juan@ferrer.com');
INSERT INTO Facebook VALUES ('Ignacio', 66, 'Madrid', 'ignacio@ferrer.com');
INSERT INTO Facebook VALUES ('Mili', 66, 'Madrid', 'mili@ferrer.com');

