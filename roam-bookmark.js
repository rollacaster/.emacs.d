javascript: location.href =
  'org-protocol://roam-ref?template=l&ref=' +
  encodeURIComponent(location.href) +
  '&title=' +
  encodeURIComponent(document.title) +
  '&body=' +
  encodeURIComponent(window.getSelection()) +
  (document.querySelector("head > meta[itemprop='author'") ||
    (document.querySelector('head > script[type="application/ld+json"]') &&
      ('&author=' +
        document.querySelector("head > meta[itemprop='author'").content ||
        JSON.parse(
          document.querySelector('head > script[type="application/ld+json"]')
            .innerText
        )
          .find((schema) => schema.author)
          .author.map((a) => a.name)
          .join(' '))))
