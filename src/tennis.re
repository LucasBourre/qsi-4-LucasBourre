type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty
  | Forty;

type pointsData = {
  playerOne: point,
  playerTwo: point
};

type fortyData = {
  player: player, /* The player who have forty points */
  otherPlayerPoint: point
};

type score =
  | Points(pointsData)
  | Forty(fortyData)
  | Deuce
  | Advantage(player)
  | Game(player);
  let p1 = Love;
  let p2 = Love;

  let scoreWhenDeuce: player => score = winner => Advantage(winner);

  let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) =>
    advantagedPlayer == winner ? Game(winner) : Deuce;
  

  let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

    /* We add a tool function to increment point */
let incrementPoint: point => option(point) =
point =>
  switch point {
  | Love => Some(Fifteen)
  | Fifteen => Some(Thirty)
  | Thirty => None
  };

  let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );

    let pointTo = (player, point, current) =>
    switch player {
    | PlayerOne => {...current, playerOne: point}
    | PlayerTwo => {...current, playerTwo: point}
    };
  
  let pointFor = (player, current) =>
    switch player {
    | PlayerOne => current.playerOne
    | PlayerTwo => current.playerTwo
    };
  
  let scoreWhenPoints = (current, winner) =>
    switch (current |> pointFor(winner) |> incrementPoint) {
    | Some(np) => Points(pointTo(winner, np, current))
    | None =>
      Forty({
        player: winner,
        otherPlayerPoint: current |> pointFor(other(winner))
      })
    };

  let scoreWhenGame = winner => Game(winner);

  let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };


  /* Exercice To do : Developper 3 fonctions :
  string_of_player , string_of_point, string_of_score */
  let string_of_player = player =>
    switch player {
    | PlayerOne => "PlayerOne"
    | PlayerTwo => "PlayerTwo"
};

let string_of_point = point =>
  switch point{
  | Love => "0"
  | Fifteen => "15"
  | Thirty => "30"
  | Forty => "40"
  };

let string_of_score = score =>
  switch score{
  | Points(p) => string_of_point(p.playerOne) ++ "-" ++ string_of_point(p.playerTwo)
  | Forty(f) => "40 - " ++ string_of_point(f.otherPlayerPoint)
  | Deuce => "40 - 40"
  | Advantage(a) => "AVANTAGE "++ string_of_player(a)
  | Game(g) => "JEU " ++ string_of_player(g)
  };

let newGame = Points({playerOne: Love, playerTwo: Love});