|build-status| |melpa-dev|

  Twitter: millions of users can't be wrong... or not.

A Gnus backend for Twitter.

.. |build-status|
   image:: https://github.com/dickmao/nntwitter/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/nntwitter/actions
   :alt: Build Status
.. |melpa-dev|
   image:: https://melpa.org/packages/nntwitter-badge.svg
   :target: http://melpa.org/#/nntwitter
   :alt: MELPA current version
.. |melpa-stable|
   image:: http://melpa-stable.milkbox.net/packages/ein-badge.svg
   :target: http://melpa-stable.milkbox.net/#/ein
   :alt: MELPA stable version

.. image:: https://github.com/dickmao/gnus-imap-walkthrough/blob/master/thumbnail.png
   :target: https://youtu.be/DMpZtC98F_M
   :alt: Replacing Thunderbird With Gnus

.. image:: screenshot.gif
.. |--| unicode:: U+2013   .. en dash
.. |---| unicode:: U+2014  .. em dash, trimming surrounding whitespace
   :trim:

Install
=======
We are trying to push ELPASO_ as the preferred package installer.
Alternatively, directly clone this repo and ``make install``.

**On 27 April 2023, Twitter actualized its promised embargo of Free
Tier applications including nntwitter.**  While the announcement to
move Twitter's API to a paid model occurred much earlier in February
2023, we only witnessed HTTP 403 (Forbidden) responses shortly after
midnight today.

We want to thank the user of nntwitter for supporting us through the years.
Yes, "user" is singular.  We also want to thank Mr. Musk for exercising
fiscal discretion in terminating free access to its API.  While we ourselves
balk at ponying 100$ per month required of Basic Access, our frugality
is not Mr. Musk's fault.  We hold out hope a sponsor will materialize to
restore nntwitter to its erstwhile glory.

Usage
=====
In your ``.emacs`` or ``init.el``, use ONE of the following:

::

   ;; Applies to first-time Gnus users
   (custom-set-variables '(gnus-select-method (quote (nntwitter ""))))

or, if you're an existing Gnus user,

::

   ;; Applies to existing Gnus users
   (add-to-list 'gnus-secondary-select-methods '(nntwitter ""))

Then ``M-x gnus``.

Initial setup should guide you through OAuth and find your existing feeds.

Select a feed via ``RET``.  Rapidly catch yourself up via ``N`` and ``P``.  Instantly catch-up with ``c``.

From the ``*Group*`` buffer, press ``g`` to refresh all feeds.  ``M-g`` on a particular feed to refresh individually.

From the summary buffer, ``/o`` redisplays articles already read.  ``x`` undisplays them.

Gnus beginners may find the interface bewildering.  In particular, feeds with no unread articles do not display.  Use ``L`` to bring them out of hiding.

.. _walkthrough: https://github.com/dickmao/gnus-imap-walkthrough
.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _Getting started: http://melpa.org/#/getting-started
.. _virtualenv: https://virtualenv.pypa.io/en/stable
.. _PRAW: https://github.com/praw-dev/praw/pull/1094
.. _ELPASO: http://github.com/dickmao/elpaso
