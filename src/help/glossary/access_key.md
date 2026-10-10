---
id: glossary.access_key
title: Project access keys (sharing a project)
audience: user
category: glossary
see_also: [project.create_new]
---

A project's **access key** is what lets someone other than its owner join it. Every
project has one, generated when the project is created; its owner finds it listed
alongside their owned projects under **Load Existing Project**. To share a project,
give a collaborator both the **Project ID** and this **Access Key** — they enter
both under **Add New Project**, and once the key validates they're added as a
collaborator (not an owner): they can work in the project, but only the owner's own
"Projects you own" list shows the key itself.

::: more
The access key is a UUID, checked against a strict format before anything is looked
up — a malformed key is rejected immediately with "Invalid UUID format," before the
app ever checks whether it matches the project. A correct key always joins you as a
collaborator, never as owner; ownership transfers aren't done through this flow.
:::
