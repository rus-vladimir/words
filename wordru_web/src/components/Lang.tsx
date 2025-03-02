import i18n from "i18next";

import {
  Select,
  SelectContent,
  SelectGroup,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";

interface LangProps {
  OnChange: (language: string) => void;
  SupportedLanguages: [lang: string, name: string][];
  Language: string;
}

export function Lang(props: LangProps) {
  const onLangChange = (lang: string) => {
    i18n.changeLanguage(lang);
    props.OnChange(lang);
  };

  return (
    <div className="absolute top-0 right-0 mt-2 mr-2 w-30">
      <Select value={props.Language} onValueChange={onLangChange}>
        <SelectTrigger className="w-30">
          <SelectValue placeholder="Lang" />
        </SelectTrigger>
        <SelectContent>
          <SelectGroup>
            {Object.entries(props.SupportedLanguages).map(
              ([_, [code, name]]) => (
                <SelectItem value={code} key={code}>
                  {name}
                </SelectItem>
              ),
            )}
          </SelectGroup>
        </SelectContent>
      </Select>
    </div>
  );
}
