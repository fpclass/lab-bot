# CS141 Lab Slack Bot

The source code for the CS141 Lab Slack bot. Usage:

```
slack-lab-bot --from CHANNEL --message TIMESTAMP [--to CHANNEL] [--module MODULE --set SMALLGROUPSET --group SMALLGROUP --event SMALLGROUPEVENT --week WEEK]
```

The bot expects an environment variable named `SLACK_TOKEN` to contain the Slack API key.

When `--to CHANNEL` is specified, a list of students in attendance will be posted to `CHANNEL`. Useful to help lab tutors identify which students are present. If this option is not specified, the list will be dumped to the standard output instead.

When `--module MODULE --set SMALLGROUPSET --group SMALLGROUP --event SMALLGROUPEVENT --week WEEK` are specified, the bot will automatically register present students' attendance on Tabula. This requires a file named `bot.json` with the Tabula user credentials to be present in the working directory.
