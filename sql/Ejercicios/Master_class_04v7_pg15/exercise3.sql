/*
3.
Write a query to obtain all the facebook friends which do not reside in Madrid.
*/

SELECT * FROM Facebook WHERE ciudad_residencia != 'Madrid';
