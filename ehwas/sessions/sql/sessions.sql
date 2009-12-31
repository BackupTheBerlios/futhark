
\i /usr/share/postgresql/contrib/pgcrypto.sql;

CREATE LANGUAGE plpgsql;

CREATE SEQUENCE uuid_sequence;

CREATE OR REPLACE FUNCTION uuid () 
RETURNS text
AS $$
   SELECT encode(
   	  digest(
		quote_literal (random ()) ||
		quote_literal (current_date) || 
		'this is a string' ||
  	  	quote_literal (nextval ('uuid_sequence')), 'sha256'), 
	  'base64')
$$ LANGUAGE SQL;

CREATE SEQUENCE sessions_seq;

CREATE TABLE sessions (
       id integer		PRIMARY KEY DEFAULT nextval ('sessions_seq'),
       uid text 		UNIQUE NOT NULL DEFAULT uuid (),
       expire_time timestamp 	DEFAULT now () + interval '1 hour',
       session_data bytea
);

CREATE INDEX sessions_expire_date 
ON sessions USING btree (expire_time);

CREATE VIEW valid_sessions 
AS SELECT * 
   FROM sessions 
   WHERE expire_time >= now ();

CREATE VIEW expired_sessions
AS SELECT *
   FROM sessions
   WHERE expire_time < now ();

CREATE RULE delete_from_expired_sessions AS 
ON DELETE TO expired_sessions
DO INSTEAD
   DELETE FROM sessions
   WHERE id = old.id;


CREATE OR REPLACE FUNCTION new_session ()
RETURNS text
AS $$
   DECLARE
   myuid text;
   BEGIN
   INSERT INTO sessions (session_data) 
   VALUES (NULL)
   RETURNING uid INTO myuid;
   
   RETURN myuid;
   END
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION get_session (text)
RETURNS bytea
AS $$
   DECLARE
   sdata bytea;
   BEGIN
   SELECT session_data 
   INTO sdata
   FROM valid_sessions
   WHERE uid = $1;

   UPDATE sessions
   SET expire_time = DEFAULT
   WHERE uid = $1;
   
   RETURN sdata;
   END
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION save_session (text, bytea) 
RETURNS void
AS $$
   BEGIN
   UPDATE sessions
   SET 
   expire_time = DEFAULT,
   session_data = $2
   WHERE uid = $1;
   END
$$ LANGUAGE plpgsql;
