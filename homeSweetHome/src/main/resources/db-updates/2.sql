-- Notification channels need to be lowercase identifiers to be listened to
DROP TRIGGER online_players_notify ON online_players;
CREATE TRIGGER online_players_notify
    AFTER INSERT OR UPDATE OR DELETE
    ON online_players
    FOR EACH ROW
EXECUTE PROCEDURE notify_trigger('homesweethome_global_players_change');

DROP TRIGGER requests_notify ON requests;
CREATE TRIGGER requests_notify
    AFTER INSERT OR UPDATE OR DELETE
    ON requests
    FOR EACH ROW
EXECUTE PROCEDURE notify_trigger('homesweethome_requests_change');

DROP TRIGGER invites_notify ON invites;
CREATE TRIGGER invites_notify
    AFTER INSERT OR UPDATE OR DELETE
    ON invites
    FOR EACH ROW
EXECUTE PROCEDURE notify_trigger('homesweethome_invites_change');

DROP TRIGGER delayed_teleports_notify ON delayed_teleports;
CREATE TRIGGER delayed_teleports_notify
    AFTER INSERT OR UPDATE OR DELETE
    ON delayed_teleports
    FOR EACH ROW
EXECUTE PROCEDURE notify_trigger('homesweethome_delayed_teleport_change');
