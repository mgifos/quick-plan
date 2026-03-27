package com.github.mgifos.workouts

/** Holds the OAuth2 bearer token for an authenticated Garmin Connect session. */
case class GarminSession(accessToken: String)

/** A workout that exists on Garmin Connect, identified by its name and server-assigned ID. */
case class GarminWorkout(name: String, id: Long)
