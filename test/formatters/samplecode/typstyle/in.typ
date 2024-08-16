#{
    let (title,
        _) = query(heading.where(level:
        1)).map(e => (
    e.body,
      e.location()
          .page(),
        )).rev().find(((_, v)) => v
        <= page)
}
