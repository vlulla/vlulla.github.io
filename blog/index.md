---
layout: default
title: Vijay's Blog
---

# Blog

{% for post in site.posts %}
- [{{ post.title }}]({{ post.url }}) -- ({{ post.date | date: "%Y.%m.%d"}})
{% endfor %}
