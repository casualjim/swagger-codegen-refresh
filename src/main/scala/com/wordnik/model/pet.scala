package com.wordnik.model

import java.util.Date

case class Tag(id: Long, name: String)
case class Category(id: Long, name: String)
case class Pet(id: Long, category: Category, status: String, name: String, photoUrls: Seq[String], tags: Seq[Tag])
case class Order(id: Long, petId: Long, status: String, quantity: Int, shipDate: Date)
case class User(id: Long, firstName: String, lastName: String, password: String, phone: String, username: String, email: String, userStatus: String)

