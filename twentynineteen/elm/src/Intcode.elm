module Intcode exposing (..)

import Array exposing (Array)
import List.Extra as List
import Parser as P exposing ((|.), (|=), Parser)


type Intcode
    = Intcode (Array Int)


type Parameter
    = Position Int
    | Immediate Int


type alias Buffer =
    { code : Intcode, marker : Int, inputs : List Int, outputs : List Int }


type Instruction
    = Terminate
    | Add Parameter Parameter Int
    | Mult Parameter Parameter Int
    | Input Int
    | Output Parameter
    | JumpIfTrue Parameter Parameter
    | JumpIfFalse Parameter Parameter
    | LessThan Parameter Parameter Int
    | Equals Parameter Parameter Int


type Op
    = OpTerm
    | OpAdd
    | OpMult
    | OpInput
    | OpOutput
    | OpJIT
    | OpJIF
    | OpLT
    | OpEq


type Mode
    = ModePos
    | ModeImm


type alias Parametrization =
    { opCode : Op, mode1 : Mode, mode2 : Mode, mode3 : Mode }



-- parametrizations appear as [0][1][0][02]


parseParametrization : Parser Parametrization
parseParametrization =
    P.succeed (\m3 m2 m1 oc -> Parametrization oc m1 m2 m3)
        |= parseMode
        |= parseMode
        |= parseMode
        |= parseOp


parseMode : Parser Mode
parseMode =
    P.succeed identity
        |= P.oneOf
            [ P.succeed ModeImm
                |. P.chompIf (\c -> c == '1')
            , P.succeed ModePos
                |. P.chompIf (\c -> c == '0')
            ]


parseOp : Parser Op
parseOp =
    P.succeed identity
        |= P.oneOf
            [ P.succeed identity
                |. P.chompIf (\c -> c == '0')
                |= P.oneOf
                    [ P.succeed OpAdd
                        |. P.chompIf (\c -> c == '1')
                    , P.succeed OpMult
                        |. P.chompIf (\c -> c == '2')
                    , P.succeed OpInput
                        |. P.chompIf (\c -> c == '3')
                    , P.succeed OpOutput
                        |. P.chompIf (\c -> c == '4')
                    , P.succeed OpJIT
                        |. P.chompIf (\c -> c == '5')
                    , P.succeed OpJIF
                        |. P.chompIf (\c -> c == '6')
                    , P.succeed OpLT
                        |. P.chompIf (\c -> c == '7')
                    , P.succeed OpEq
                        |. P.chompIf (\c -> c == '8')
                    ]
            , P.succeed OpTerm
                |. P.chompIf (\c -> c == '9')
                |. P.chompIf (\c -> c == '9')
            ]


wrapInParam : ( Mode, Int ) -> Parameter
wrapInParam ( mode, int ) =
    case mode of
        ModeImm ->
            Immediate int

        ModePos ->
            Position int


readInstruction : List Int -> Instruction
readInstruction block =
    case block of
        parametz :: params ->
            case P.run parseParametrization (String.padLeft 5 '0' <| String.fromInt parametz) of
                Ok { mode1, mode2, mode3, opCode } ->
                    let
                        zipped =
                            List.zip [ mode1, mode2, mode3 ] params
                    in
                    case ( opCode, zipped ) of
                        ( OpInput, ( _, pos ) :: _ ) ->
                            Input pos

                        ( OpOutput, p :: _ ) ->
                            Output (wrapInParam p)

                        ( OpAdd, p1 :: p2 :: ( _, i3 ) :: _ ) ->
                            Add (wrapInParam p1) (wrapInParam p2) i3

                        ( OpMult, p1 :: p2 :: ( _, i3 ) :: _ ) ->
                            Mult (wrapInParam p1) (wrapInParam p2) i3

                        ( OpJIT, p1 :: p2 :: _ ) ->
                            JumpIfTrue (wrapInParam p1) (wrapInParam p2)

                        ( OpJIF, p1 :: p2 :: _ ) ->
                            JumpIfFalse (wrapInParam p1) (wrapInParam p2)

                        ( OpLT, p1 :: p2 :: ( _, i3 ) :: _ ) ->
                            LessThan (wrapInParam p1) (wrapInParam p2) i3

                        ( OpEq, p1 :: p2 :: ( _, i3 ) :: _ ) ->
                            Equals (wrapInParam p1) (wrapInParam p2) i3

                        _ ->
                            Terminate

                Err _ ->
                    Terminate

        [] ->
            Terminate


readInstruction2 : List Int -> Instruction
readInstruction2 block =
    case block of
        99 :: _ ->
            Terminate

        [ 1, x, y, resPos ] ->
            Add (Position x) (Position y) resPos

        [ 2, x, y, resPos ] ->
            Mult (Position x) (Position y) resPos

        3 :: writePos :: _ ->
            Input writePos

        4 :: readPos :: _ ->
            Output (Position readPos)

        _ ->
            Terminate


evaulateParameter : Parameter -> Intcode -> Maybe Int
evaulateParameter parameter intcode =
    case ( parameter, intcode ) of
        ( Immediate i, _ ) ->
            Just i

        ( Position pos, Intcode a ) ->
            Array.get pos a


doOperation : (Int -> Int -> Int) -> Parameter -> Parameter -> Int -> Intcode -> Maybe Intcode
doOperation op param1 param2 i ic =
    case ic of
        Intcode a ->
            evaulateParameter param1 ic
                |> Maybe.andThen
                    (\x ->
                        evaulateParameter param2 ic
                            |> Maybe.map
                                (\y ->
                                    Array.set i (op x y) a |> Intcode
                                )
                    )


jumpHelper : Parameter -> Parameter -> Intcode -> Bool -> Int -> Maybe Int
jumpHelper param1 param2 ic isTrueJump marker =
    let
        satisfied i =
            if isTrueJump then
                i /= 0

            else
                i == 0
    in
    evaulateParameter param1 ic
        |> Maybe.andThen
            (\i1 ->
                if satisfied i1 then
                    evaulateParameter param2 ic

                else
                    Just <| marker + 3
            )


compareHelper : Parameter -> Parameter -> Intcode -> (Int -> Int -> Bool) -> Maybe Bool
compareHelper param1 param2 ic comparer =
    evaulateParameter param1 ic
        |> Maybe.andThen
            (\i1 ->
                evaulateParameter param2 ic
                    |> Maybe.map
                        (\i2 ->
                            comparer i1 i2
                        )
            )


runHelper : Buffer -> List Int
runHelper ({ code, marker, inputs, outputs } as buffer) =
    case code of
        Intcode a ->
            let
                instruction =
                    readInstruction (Array.slice marker (marker + 4) a |> Array.toList)
            in
            case instruction of
                Terminate ->
                    outputs

                Add param1 param2 i ->
                    case doOperation (+) param1 param2 i code of
                        Just newCode ->
                            runHelper { buffer | code = newCode, marker = marker + 4 }

                        Nothing ->
                            outputs

                Mult param1 param2 i ->
                    case doOperation (*) param1 param2 i code of
                        Just newCode ->
                            runHelper { buffer | code = newCode, marker = marker + 4 }

                        Nothing ->
                            outputs

                Input i ->
                    let
                        ( newCode, newInputs ) =
                            case ( inputs, code ) of
                                ( [], _ ) ->
                                    ( code, inputs )

                                ( input :: otherInputs, Intcode arr ) ->
                                    ( Intcode <| Array.set i input arr, otherInputs )
                    in
                    runHelper { buffer | marker = marker + 2, code = newCode, inputs = newInputs }

                Output param ->
                    let
                        newOutputs =
                            case code of
                                Intcode arr ->
                                    case param of
                                        Position i ->
                                            case Array.get i arr of
                                                Just value ->
                                                    value :: outputs

                                                Nothing ->
                                                    outputs

                                        Immediate v ->
                                            v :: outputs
                    in
                    runHelper { buffer | marker = marker + 2, outputs = newOutputs }

                JumpIfTrue param1 param2 ->
                    let
                        maybeNewMarker =
                            jumpHelper param1 param2 code True marker
                    in
                    case maybeNewMarker of
                        Just newMarker ->
                            runHelper { buffer | marker = newMarker }

                        Nothing ->
                            outputs

                JumpIfFalse param1 param2 ->
                    let
                        maybeNewMarker =
                            jumpHelper param1 param2 code False marker
                    in
                    case maybeNewMarker of
                        Just newMarker ->
                            runHelper { buffer | marker = newMarker }

                        Nothing ->
                            outputs

                LessThan param1 param2 i ->
                    case compareHelper param1 param2 code (<) of
                        Just b ->
                            let
                                val =
                                    if b then
                                        1

                                    else
                                        0

                                newCode =
                                    case code of
                                        Intcode arr ->
                                            Array.set i val arr |> Intcode
                            in
                            runHelper { buffer | marker = marker + 4, code = newCode }

                        Nothing ->
                            outputs

                Equals param1 param2 i ->
                    case compareHelper param1 param2 code (==) of
                        Just b ->
                            let
                                val =
                                    if b then
                                        1

                                    else
                                        0

                                newCode =
                                    case code of
                                        Intcode arr ->
                                            Array.set i val arr |> Intcode
                            in
                            runHelper { buffer | marker = marker + 4, code = newCode }

                        Nothing ->
                            outputs


run_ : List Int -> Intcode -> List Int
run_ inputs ic =
    runHelper { code = ic, marker = 0, inputs = inputs, outputs = [] }


runFromArray : List Int -> Array Int -> List Int
runFromArray inputs arr =
    run_ inputs <| Intcode arr
