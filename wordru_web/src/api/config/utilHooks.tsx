import { RefObject, useRef } from "react";

const useFocus = (): [any, () => void] => {
  const htmlElRef: RefObject<any> = useRef(null);
  const setFocus = (): void => {
    htmlElRef?.current?.focus?.();
  };

  return [htmlElRef, setFocus];
};

export default useFocus;
