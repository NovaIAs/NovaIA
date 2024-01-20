```sql
-- Esta consulta elimina todos los registros de la tabla `users` que no se utilizaron durante al menos 3 meses.

DELETE FROM users
WHERE last_login_date < DATE_SUB(NOW(), INTERVAL 3 MONTH);

-- Esta consulta crea una nueva vista llamada `active_users` que incluye los campos de la tabla `users`, filtrados por aquellos que se utilizaron durante los últimos 7 días.

CREATE VIEW active_users AS
SELECT *
FROM users
WHERE last_login_date >= DATE_SUB(NOW(), INTERVAL 7 DAY);

-- Esta consulta crea una función escalar llamada `get_user_name` que devuelve el nombre completo de un usuario dado su identificador.

CREATE FUNCTION get_user_name(user_id INT) RETURNS VARCHAR(255)
BEGIN
    DECLARE user_name VARCHAR(255);

    -- Obtiene el nombre del usuario de la base de datos.
    SELECT name INTO user_name FROM users WHERE id = user_id;

    -- Devuelve el nombre completo del usuario.
    RETURN user_name;
END;

-- Este procedimiento almacenado llamado `update_user_email` actualiza el campo de correo electrónico de un usuario dado su identificador y su nueva dirección de correo electrónico.

CREATE PROCEDURE update_user_email(user_id INT, new_email VARCHAR(255))
BEGIN
    -- Actualiza el correo electrónico del usuario en la base de datos.
    UPDATE users SET email = new_email WHERE id = user_id;

    -- Obtiene el nombre del usuario.
    DECLARE user_name VARCHAR(255);
    SELECT name INTO user_name FROM users WHERE id = user_id;

    -- Envía un correo electrónico al usuario para confirmar el cambio de su dirección de correo electrónico.
    SEND EMAIL TO user_name@example.com
    FROM noreply@example.com
    SUBJECT "Confirmación de cambio de correo electrónico"
    BODY "Se ha cambiado su dirección de correo electrónico a " || new_email;
END;

-- Este activador se dispara antes de que se elimine una fila de la tabla `users`.

CREATE TRIGGER delete_user_trigger BEFORE DELETE ON users
FOR EACH ROW
BEGIN
    -- Envía un correo electrónico al usuario para informarle de que su cuenta ha sido eliminada.
    SEND EMAIL TO OLD.email
    FROM noreply@example.com
    SUBJECT "Eliminación de su cuenta"
    BODY "Su cuenta ha sido eliminada.";

    -- Elimina todos los registros asociados con el usuario de las tablas relacionadas.
    DELETE FROM orders WHERE user_id = OLD.id;
    DELETE FROM reviews WHERE user_id = OLD.id;
END;

-- Este disparador se dispara después de que se actualice una fila de la tabla `users`.

CREATE TRIGGER update_user_trigger AFTER UPDATE ON users
FOR EACH ROW
BEGIN
    -- Si se ha actualizado el campo de correo electrónico del usuario, se envía un correo electrónico