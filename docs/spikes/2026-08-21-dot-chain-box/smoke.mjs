import { firefox, KEY } from "./bidi.mjs";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
const HERE = dirname(fileURLToPath(import.meta.url));
const ff = await firefox();
await ff.goto(pathToFileURL(join(HERE, "g-sql.html")).href);
console.log(await ff.eval(() => ({
  from1: RIG.stageString("from", "work"),
  from2: RIG.stageString("from", "work, home"),
  fromAll: [RIG.stageString("from", "*"), RIG.stageString("from", "all"),
            RIG.stageString("from", "default")],
  fromUnknown: RIG.stageString("from", "nosuch"),
  star: RIG.stageString("columns", "*"),
  starIr: RIG.irSql("SELECT *"),
  handIr: RIG.irSql("SELECT state, priority, title, scheduled, deadline, closed, tags"),
  sixIr: RIG.irSql("SELECT state, priority, title, scheduled, deadline, tags"),
  noneIr: RIG.irFlat(""),
  custom: RIG.stageString("columns", "owner, title"),
  customIr: [RIG.irSql("SELECT owner, title"), RIG.irFlat("columns:owner,Title")],
  fromIr: [RIG.irSql("SELECT * FROM work"), RIG.irFlat("tag:work columns:State,#,Title,Scheduled,Deadline,Closed,Tags")],
  unionIr: [RIG.irSql("FROM work, home"), RIG.irFlat("tag:work|home")],
  bothIr: [RIG.irSql("FROM work WHERE tag = 'urgent'"), RIG.irFlat("tag:work tag:urgent")],
  omitted: [RIG.irSql("WHERE state = ACTIVE"), RIG.irSql("FROM all WHERE state = ACTIVE"),
            RIG.irFlat("state:*active*")],
  stmt: RIG.sqlStatementOf("state:*active* -tag:chore"),
  trip: RIG.irSql(RIG.sqlStatementOf("state:*active* -tag:chore sort:deadline")),
  tripFlat: RIG.irFlat("state:*active* -tag:chore sort:deadline"),
  cols: RIG.cols(),
})));
await ff.close();
