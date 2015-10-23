module Airship.Config
    ( AirshipConfig
    , HeaderInclusion (..)
    , includeTraceHeader
    , includeQuipHeader
    , defaultAirshipConfig
    ) where

import           Lens.Micro (Lens', lens)

-- | An opaque data type encapsulating all Airship-specific configuration options.
--
-- We use lenses to modify 'AirshipConfig' values -- though Airship only depends on the
-- microlens library, its lenses are compatible with Control.Lens.
data AirshipConfig = AirshipConfig
    { _includeTraceHeader :: HeaderInclusion
    , _includeQuipHeader :: HeaderInclusion
    }

data HeaderInclusion = IncludeHeader | OmitHeader deriving (Eq, Show)

-- | Determines whether or not the @Airship-Trace@ header, which traces the execution of
-- a given request in the Airship decision tree, is included in every HTTP response.
-- While exposing the decision tree is usually innocuous (and makes for significantly easier
-- debugging), you may want to turn it off in certain circumstances.
--
-- Defaults to 'IncludeHeader' (enabled).
includeTraceHeader :: Lens' AirshipConfig HeaderInclusion
includeTraceHeader = lens _includeTraceHeader (\s n -> s { _includeTraceHeader = n })

-- | Determines whether or not the @Airship-Quip@ header, which includes a pithy
-- quote in your response headers, is included in every HTTP response.
--
-- Defaults to 'IncludeHeader' (enabled).
includeQuipHeader :: Lens' AirshipConfig HeaderInclusion
includeQuipHeader = lens _includeQuipHeader (\s n -> s { _includeQuipHeader = n })

-- | The default configuration. Use this, in conjunction with the lenses declared
-- above, to get and modify an 'AirshipConfig' to pass to 'resourceToWai'.
defaultAirshipConfig :: AirshipConfig
defaultAirshipConfig = AirshipConfig IncludeHeader IncludeHeader
