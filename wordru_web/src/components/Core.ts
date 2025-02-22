export enum LetterPresence {
    CorrectPosition,
    WrongPossition,
    Missing,
    Empty,
  }

export function getLetterColor(p: LetterPresence) {
  switch (p) {
    case LetterPresence.Missing:
      return "bg-gray-500 hover:bg-gray-400 focus:bg-gray-600";
    case LetterPresence.WrongPossition:
      return "bg-orange-500 hover:bg-orange-400 focus:bg-orange-600";
    case LetterPresence.CorrectPosition:
      return "bg-green-500 hover:bg-green-400 focus:bg-green-600";
    default:
      return "bg-zinc-900 hover:bg-zinc-800 focus:bg-zinc-950 dark:bg-neutral-950 dark:hover:bg-neutral-700 dark:focus:bg-black";
  }
}
