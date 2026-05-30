---
layout: default
title: Vijay's Blog
---

# Blog

{% for post in site.posts %}
- {{ post.date | date: "%Y.%m.%d"}}: [{{ post.title }}]({{ post.url }})
{% endfor %}
