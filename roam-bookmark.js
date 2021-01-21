javascript: location.href =
  'org-protocol://roam-ref?template=l&ref=' +
  encodeURIComponent(location.href) +
  '&title=' +
  encodeURIComponent(document.title) +
  '&body=' +
  encodeURIComponent(window.getSelection()) +
  (document.querySelector("head > meta[itemprop='author'")
    ? '&author=' +
      document.querySelector("head > meta[itemprop='author'").content
    : document.querySelector('head > script[type="application/ld+json"]') &&
      Array.isArray(
        JSON.parse(
          document.querySelector('head > script[type="application/ld+json"]')
            .innerText
        )
      )
    ? '&author=' +
      Array.isArray(
        JSON.parse(
          document.querySelector('head > script[type="application/ld+json"]')
            .innerText
        )
      )
        .find((schema) => schema.author)
        .author.map((a) => a.name)
        .join(' ')
    : document.querySelector('head > script[type="application/ld+json"]') &&
      JSON.parse(
        document.querySelector('head > script[type="application/ld+json"]')
          .innerText
      ).author
    ? '&author=' +
      JSON.parse(
        document.querySelector('head > script[type="application/ld+json"]')
          .innerText
      ).author
    : '')
