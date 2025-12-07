import NavBar from "@/components/nav-bar";
import WorkSpace from "@/components/work-space";

export default function Home() {
  return (
    <div className="flex h-screen w-screen flex-col">
      <NavBar />
      <WorkSpace />
    </div>
  );
}
