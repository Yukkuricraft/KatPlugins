package net.katsstuff.bukkit.katlib.db.testing

/** The notify trigger function the plugins create in their first database update. */
object NotifySql:
  val notifyTriggerFunction: String =
    """CREATE OR REPLACE FUNCTION notify_trigger() RETURNS trigger as
      |$triger$
      |DECLARE
      |    rec           RECORD;;
      |    dat           RECORD;;
      |    payload       TEXT;;
      |    inner_payload JSON;;
      |BEGIN
      |    CASE tg_op
      |        WHEN 'UPDATE' THEN rec := NEW;;
      |                           dat := OLD;;
      |                           inner_payload := json_build_object('new', row_to_json(rec), 'old', row_to_json(dat));;
      |        WHEN 'INSERT' THEN rec := NEW;;
      |                           inner_payload := row_to_json(rec);;
      |        WHEN 'DELETE' THEN rec := OLD;;
      |                           inner_payload := row_to_json(rec);;
      |        ELSE RAISE EXCEPTION 'Unknown TG_OP: "%". Should not occur!', TG_OP;;
      |        END CASE;;
      |
      |    payload := json_build_object('action', tg_op, 'payload', inner_payload);;
      |
      |    PERFORM pg_notify(TG_ARGV[0], payload);;
      |
      |    RETURN rec;;
      |end;;
      |$triger$
      |    LANGUAGE plpgsql;""".stripMargin
