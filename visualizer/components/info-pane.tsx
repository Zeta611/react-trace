import { Slider } from "@/components/retroui/Slider";

export default function InfoPane() {
  return (
    <div className="flex h-full items-center justify-center">
      <Slider
        className="w-full mx-5"
        defaultValue={[0]}
        min={0}
        max={10}
        step={1}
        aria-label="Execution Step Slider"
      />
    </div>
  );
}
