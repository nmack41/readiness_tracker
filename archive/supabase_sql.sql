# Create table with metrics to track

CREATE TABLE training_metrics (
    id SERIAL PRIMARY KEY,
    weight SMALLINT NOT NULL,
    sleep SMALLINT NOT NULL,
    motivation_to_train SMALLINT NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);


# Enable Row Level Security (RLS)
ALTER TABLE public.training_metrics ENABLE ROW LEVEL SECURITY;


-- Create read access policy
CREATE POLICY "owner read access"
ON public.training_metrics
FOR SELECT
USING (user_id = auth.uid());

-- Create insert access policy
CREATE POLICY "owner insert access"
ON public.training_metrics
FOR INSERT
WITH CHECK (user_id = auth.uid());

-- Create update access policy
CREATE POLICY "owner update access"
ON public.training_metrics
FOR UPDATE
USING (user_id = auth.uid());

-- Create delete access policy
CREATE POLICY "owner delete access"
ON public.training_metrics
FOR DELETE
USING (user_id = auth.uid());


# auth.users
-- Enable RLS
alter table profiles enable row level security;

-- Create Insert Policy
CREATE POLICY "Users can create their own profile."
ON profiles
FOR INSERT
TO authenticated
WITH CHECK (id = auth.uid());


-- Create Select Policy
CREATE POLICY "Users can see their own profile only."
ON profiles
FOR SELECT
USING (id = auth.uid());


-- Create Update Policy
CREATE POLICY "Users can update their own profile."
ON profiles
FOR UPDATE
TO authenticated
USING (id = auth.uid())
WITH CHECK (id = auth.uid());



