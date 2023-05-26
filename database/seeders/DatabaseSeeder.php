<?php

namespace Database\Seeders;

use Illuminate\Database\Seeder;

class DatabaseSeeder extends Seeder
{
    /**
     * Seed the application's database.
     *
     * @return void
     */
    public function run()
    {
        \App\Models\Author::factory(10)->create();
        \App\Models\Article::factory(30)->create();
        \App\Models\Post::factory(30)->create();
        \App\Models\Book::factory(10)->create();
    }
}
