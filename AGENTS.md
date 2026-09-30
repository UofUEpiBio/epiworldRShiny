# Agent Notes

## Git Commits

When the user asks an AI agent to interact with git and the agent's work is
pertinent to a commit, the agent should add itself as a commit coauthor.

Use a `Co-authored-by:` trailer in the commit message. For Codex-authored work,
use:

```text
Co-authored-by: Codex <codex@openai.com>
```
