CREATE TABLE warps
(
    name                TEXT             NOT NULL,
    x                   DOUBLE PRECISION NOT NULL,
    y                   DOUBLE PRECISION NOT NULL,
    z                   DOUBLE PRECISION NOT NULL,
    yaw                 REAL             NOT NULL,
    pitch               REAL             NOT NULL,
    world_uuid          uuid             NOT NULL,
    server              TEXT             NOT NULL,
    display_name        JSONB,
    groups              TEXT[]           NOT NULL,
    allowed_perm_groups TEXT[]           NOT NULL,
    allowed_users       uuid             NOT NULL,
    lore                JSONB
);


-- https://gist.github.com/goliatone/5fbeb1912e5937e8e3cf94618be9bebf
CREATE OR REPLACE FUNCTION notify_trigger() RETURNS trigger as
$triger$
DECLARE
    rec           RECORD;;
    dat           RECORD;;
    payload       TEXT;;
    inner_payload JSON;;
BEGIN
    CASE tg_op
        WHEN 'UPDATE' THEN rec := NEW;;
                           dat := OLD;;
                           inner_payload := json_build_object('new', row_to_json(rec), 'old', row_to_json(dat));;
        WHEN 'INSERT' THEN rec := NEW;;
                           inner_payload := row_to_json(rec);;
        WHEN 'DELETE' THEN rec := OLD;;
                           inner_payload := row_to_json(rec);;
        ELSE RAISE EXCEPTION 'Unknown TG_OP: "%". Should not occur!', TG_OP;;
        END CASE;;

    payload := json_build_object('action', tg_op, 'payload', inner_payload);;

    PERFORM pg_notify(TG_ARGV[0], payload);;

    RETURN rec;;
end;;
$triger$
    LANGUAGE plpgsql;

CREATE TABLE delayed_teleports
(
    uuid       uuid             NOT NULL PRIMARY KEY,
    x          DOUBLE PRECISION NOT NULL,
    y          DOUBLE PRECISION NOT NULL,
    z          DOUBLE PRECISION NOT NULL,
    yaw        FLOAT            NOT NULL,
    pitch      FLOAT            NOT NULL,
    world_uuid uuid             NOT NULL,
    server     TEXT             NOT NULL,
    expires    TIMESTAMPTZ      not null
);

CREATE TRIGGER delayed_teleports_notify
    AFTER INSERT OR UPDATE OR DELETE
    ON delayed_teleports
    FOR EACH ROW
EXECUTE PROCEDURE notify_trigger('MagicalWarps.DelayedTeleportChange');